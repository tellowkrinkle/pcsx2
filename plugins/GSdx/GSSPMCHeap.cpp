/*  PCSX2 - PS2 Emulator for PCs
 *  Copyright (C) 2002-2021 PCSX2 Dev Team
 *
 *  PCSX2 is free software: you can redistribute it and/or modify it under the terms
 *  of the GNU Lesser General Public License as published by the Free Software Found-
 *  ation, either version 3 of the License, or (at your option) any later version.
 *
 *  PCSX2 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 *  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *  PURPOSE.  See the GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License along with PCSX2.
 *  If not, see <http://www.gnu.org/licenses/>.
 */

#include "stdafx.h"
#include "GSSPMCHeap.h"

namespace
{
	/// Align `value` to `align` bytes
	template <size_t align>
	size_t alignTo(size_t value)
	{
		return ((value + (align - 1)) / align) * align;
	}

	/// Align to a power of 2 using a mask that's equal to that value - 1
	size_t alignUsingMask(size_t align_mask, size_t value)
	{
		return (value + align_mask) & ~align_mask;
	}
} // namespace

/// GSSPMCHeap operates as a ring buffer, with usage counters for each quadrant
/// If a new quadrant needs to be used but is still in use by existing allocations,
///   the buffer is orphaned and a replaced with a new, larger buffer
struct GSSPMCHeap::Buffer
{
	friend class GSSPMCHeap;

	static const size_t BEGINNING_OFFSET;

	static constexpr size_t USAGE_ARR_SIZE = sizeof(uint64_t) / sizeof(size_t);
	static constexpr size_t USAGE_ARR_ELEMS_PER_ENTRY = sizeof(size_t) / sizeof(uint16_t);

	/// Refcount, main heap holds onto buffer with a +1
	/// Each allocation adds `sizeof(allocation)`, to allow for detection of buffers being used inefficiently
	///   (e.g. if a buffer is orphaned with very little usage that means allocations aren't being freed in a similar order to being made)
	/// Buffer is freed when the main heap drops it (-1) and every allocation is freed, causing this to reach 0
	std::atomic<size_t> m_amt_allocated;
	/// Holds 4x 16-bit usage counters, indicating how many allocations have been made from the nth quadrant of memory
	/// Merged into `size_t` chunks so that they can be operated on with fewer atomic operations
	std::atomic<size_t> m_usage[USAGE_ARR_SIZE];
	/// Size of whole buffer (including header)
	/// Should be kept to at least 4x the largest allocation (so each allocation touches at most 2 quadrants)
	size_t m_size;
	/// Offset of new allocations
	size_t m_write_loc;
	/// Amount to rshift buffer offset to get which quadrant it's in (`log2(m_size/4)`)
	int m_quadrant_shift;

	/// Increment usage counts (use when allocating)
	void beginUse(uint64_t usage)
	{
		for (size_t i = 0; i < USAGE_ARR_SIZE; i++)
		{
			size_t piece = static_cast<size_t>(usage >> (i * (64 / USAGE_ARR_SIZE)));
			size_t prev = m_usage[i].fetch_add(piece, std::memory_order_relaxed);
			for (size_t j = 0; j < USAGE_ARR_ELEMS_PER_ENTRY; j++)
			{
				[[maybe_unused]] uint16_t section = prev >> (j * 16);
				assert(section != UINT16_MAX && "Usage count overflow");
			}
		}
	}

	/// Decrement usage counts (use when freeing)
	void endUse(uint64_t usage)
	{
		for (size_t i = 0; i < USAGE_ARR_SIZE; i++)
		{
			size_t piece = static_cast<size_t>(usage >> (i * (64 / USAGE_ARR_SIZE)));
			m_usage[i].fetch_sub(piece, std::memory_order_release);
		}
	}

	/// Check if the given quadrant is still in use
	bool isStillInUse(uint32_t quadrant)
	{
		int arridx = (quadrant / USAGE_ARR_ELEMS_PER_ENTRY) % USAGE_ARR_SIZE;
		int shift = (quadrant % USAGE_ARR_ELEMS_PER_ENTRY) * 16;
		return ((m_usage[arridx].load(std::memory_order_acquire) >> shift) & 0xFFFF) != 0;
	}

	uint32_t quadrant(size_t off)
	{
		return static_cast<uint32_t>(off >> m_quadrant_shift);
	}

	/// Calculate a usage mask from an offset + size
	uint64_t usageMask(size_t beginOff, size_t size)
	{
		// We guarantee size <= one quadrant
		// Therefore we only need to check beginning and end
		uint64_t mask = 0;
		mask |= 1ull << (quadrant(beginOff) * 16);
		size_t endOff = beginOff + size - 1;
		mask |= 1ull << (quadrant(endOff) * 16);
		return mask;
	}

	/// Decrement the main amt_allocated refcount
	void decref(size_t amt)
	{
		if (m_amt_allocated.fetch_sub(amt, std::memory_order_release) == amt)
		{
			std::atomic_thread_fence(std::memory_order_acquire);
			vmfree(this, m_size);
		}
	}

	/// Free an allocation
	void free(void* allocation, size_t size)
	{
		const char* base = reinterpret_cast<const char*>(this);
		size_t beginOff = static_cast<const char*>(allocation) - base;
		endUse(usageMask(beginOff, size));
		decref(size);
	}

	/// Allocate a value of `size` bytes with `prefix_size` bytes before it (for allocation tracking) and alignment specified by `align_mask`
	void* alloc(size_t size, size_t align_mask, size_t prefix_size)
	{
		uint32_t prevQuadrant = quadrant(m_write_loc - 1);
		size_t baseOff = alignUsingMask(align_mask, m_write_loc + prefix_size);
		uint32_t newQuadrant = quadrant(baseOff + size - 1);
		if (newQuadrant >= 4)
		{
			newQuadrant = 0;
			baseOff = alignUsingMask(align_mask, BEGINNING_OFFSET + prefix_size);
		}
		if ((prevQuadrant != newQuadrant) && isStillInUse(newQuadrant))
			return nullptr;

		m_write_loc = baseOff + size;
		beginUse(usageMask(baseOff - prefix_size, size + prefix_size));
		m_amt_allocated.fetch_add(size + prefix_size, std::memory_order_relaxed);
		return reinterpret_cast<char*>(this) + baseOff - prefix_size;
	}

	static Buffer* make(int quadrant_shift)
	{
		size_t size = 4 << quadrant_shift;
		Buffer* buffer = reinterpret_cast<Buffer*>(vmalloc(size, false));
		buffer->m_size = size;
		buffer->m_quadrant_shift = quadrant_shift;
		buffer->m_amt_allocated.store(1, std::memory_order_relaxed);
		for (std::atomic<size_t>& usage : buffer->m_usage)
			usage.store(0, std::memory_order_relaxed);
		buffer->m_write_loc = BEGINNING_OFFSET;
		return buffer;
	}
};

const size_t GSSPMCHeap::Buffer::BEGINNING_OFFSET = alignTo<64>(sizeof(Buffer));
constexpr size_t GSSPMCHeap::MIN_ALIGN;

GSSPMCHeap::GSSPMCHeap()
{
	m_current_buffer = Buffer::make(14); // Start with 64k buffer
}

GSSPMCHeap::~GSSPMCHeap() noexcept
{
	orphanBuffer();
}

void GSSPMCHeap::orphanBuffer() noexcept
{
	m_current_buffer->decref(1);
}

void* GSSPMCHeap::alloc_internal(size_t size, size_t align_mask, size_t prefix_size)
{
	prefix_size += sizeof(Buffer*); // Add space for a pointer to the buffer
	size_t first_quad_space = alignUsingMask(align_mask, Buffer::BEGINNING_OFFSET + prefix_size) + size;

	if (first_quad_space <= (m_current_buffer->m_size / 4))
	{
		if (void* ptr = m_current_buffer->alloc(size, align_mask, prefix_size))
		{
			Buffer** bptr = static_cast<Buffer**>(ptr);
			*bptr = m_current_buffer;
			return bptr + 1;
		}
		else
		{
			size_t total = m_current_buffer->m_size;
			size_t mb = 1024 * 1024;
			if (total >= mb)
			{
				size_t used = m_current_buffer->m_amt_allocated.load(std::memory_order_relaxed) - 1;
				if (used * 4 < total)
				{
					fprintf(stderr, "GSSPMCHeap: Orphaning %dmb buffer with low usage of %d%%, check that allocations are actually being deallocated approximately in order\n", total / mb, static_cast<int>((used * 100) / total));
				}
			}
		}
	}

	// Couldn't allocate, orphan buffer and make a new one
	int shift = m_current_buffer->m_quadrant_shift;
	do
	{
		shift++;
	} while (first_quad_space > (1 << shift));

	shift = std::min(shift, 24); // If this needs to be >64 mb, we're doing something wrong
	Buffer* new_buffer = Buffer::make(shift);
	orphanBuffer();
	m_current_buffer = new_buffer;
	void* ptr = m_current_buffer->alloc(size, align_mask, prefix_size);
	assert(ptr && "Fresh buffer failed to allocate!");

	Buffer** bptr = static_cast<Buffer**>(ptr);
	*bptr = m_current_buffer;
	return bptr + 1;
}

void GSSPMCHeap::free_internal(void* ptr, size_t size) noexcept
{
	size += sizeof(Buffer*);
	Buffer** bptr = static_cast<Buffer**>(ptr) - 1;
	(*bptr)->free(bptr, size);
}
