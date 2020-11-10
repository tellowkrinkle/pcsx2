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

#include "GSRingBufferMTL.h"

#if ! __has_feature(objc_arc)
# error "Compile this with -fobjc-arc"
#endif

#ifdef __APPLE__

void GSRingBufferMTL::GrowBuffer(id<MTLDevice> device, size_t min_size)
{
	do
	{
		m_len_log += 1;
	} while (BufferLen() < min_size);
	m_lock = {};
	m_buffer = [device newBufferWithLength:BufferLen() options:MTLResourceStorageModeManaged];
	m_buffer_ptr = static_cast<uint8*>(m_buffer.contents);
	m_write_start = 0;
}

[[clang::optnone]]
void* GSRingBufferMTL::Map(id<MTLCommandBuffer> target, size_t len)
{
	len = (len + 15ULL) & ~15ULL; // Align len
	if (len > BufferLen())
		GrowBuffer(target.device, len);
	if (m_write_start + len > BufferLen())
		m_write_start = 0;
	size_t quarter_len_log = m_len_log - 2;
	size_t quarter_len = 1ULL << quarter_len_log;

	// If we write to a portion managed by lock 1, then to a portion managed by locks 1 and 2, we want to update locks 1 and 2 but only grow if lock 2 is still locked
	// Therefore we should only check the lock containing m_write_start if it's the very beginning of the lock's region
	size_t lock_check_start = (m_write_start + quarter_len - 1) >> quarter_len_log;
	size_t lock_end = (m_write_start + len - 1) >> quarter_len_log;
	for (size_t i = lock_check_start; i <= lock_end; i++)
	{
		if (m_lock[i] && m_lock[i].GPUEndTime == 0)
		{
			// The section managed by this lock is still in use by the GPU!  Orphan the old buffer and make a new, bigger, one
			GrowBuffer(target.device, len);
			// GrowBuffer updates m_write_start
			lock_end = (len - 1) >> quarter_len_log;
			break;
		}
	}

	size_t lock_start = m_write_start >> quarter_len_log;

	// Update locks
	// Assumes a later-uploaded piece of data will always be for a command buffer that is executed at the same time or later than the last
	for (size_t i = lock_start; i <= lock_end; i++)
	{
		// Minor refcount optimization
		if (m_lock[i] != target)
			m_lock[i] = target;
	}

	m_current_map_len = len;
	return m_buffer_ptr + m_write_start;
}

std::pair<id<MTLBuffer>, size_t> GSRingBufferMTL::Unmap()
{
	std::pair<id<MTLBuffer>, size_t> o { m_buffer, m_write_start };
	[m_buffer didModifyRange:NSMakeRange(m_write_start, m_current_map_len)];
	m_write_start += m_current_map_len;
	return o;
}

std::pair<id<MTLBuffer>, size_t> GSRingBufferMTL::Upload(id<MTLCommandBuffer> target, const void* data, size_t len)
{
	void* buf = Map(target, len);
	memcpy(buf, data, len);
	return Unmap();
}

#endif
