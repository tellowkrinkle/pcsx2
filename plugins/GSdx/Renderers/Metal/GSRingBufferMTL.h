/*
 *	Copyright (C) 2020 PCSX2 Dev Team
 *
 *  This Program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This Program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with GNU Make; see the file COPYING.  If not, write to
 *  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA USA.
 *  http://www.gnu.org/copyleft/gpl.html
 *
 */

#pragma once

#ifdef __APPLE__

#include <Metal/Metal.h>
#include <utility>
#include <array>

/// A `MTLBuffer` ring buffer
struct GSRingBufferMTL
{
	/// The currently-in-use MTLBuffer
	id<MTLBuffer> m_buffer = nil;
	/// Pointer to the contents of `m_buffer` for fast access
	uint8* m_buffer_ptr = nullptr;
	/// The log2 of m_buffer's size
	size_t m_len_log = 0;
	size_t m_write_start = 0;
	size_t m_current_map_len;
	std::array<id<MTLCommandBuffer>, 4> m_lock;

	size_t BufferLen() { return 1 << m_len_log; }
	void GrowBuffer(id<MTLDevice> device, size_t min_size);

public:
	GSRingBufferMTL() = default;

	/// Get a pointer to manually update data
	void* Map(id<MTLCommandBuffer> target, size_t len);

	/// Indicate that you finished updating data from `Map`
	/// Returns a MTLBuffer and an offset into that buffer representing the data you just updated on the GPU
	/// `len` must be less than or equal to the size you specified to `Map`
	std::pair<id<MTLBuffer>, size_t> Unmap();

	/// Schedule a piece of data to be uploaded to the GPU
	/// Returns a MTLBuffer and an offset into that buffer
	/// Note: The buffer from different calls to `Upload` on the same ring buffer may differ
	std::pair<id<MTLBuffer>, size_t> Upload(id<MTLCommandBuffer> target, const void* data, size_t len);
};

#endif
