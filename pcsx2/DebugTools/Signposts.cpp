/*  PCSX2 - PS2 Emulator for PCs
 *  Copyright (C) 2020  PCSX2 Dev Team
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

#include "Signposts.h"

#if defined(__APPLE__) && SIGNPOST_USE_OSLOG && defined(__MAC_10_14)

static os_log_t makeOSLog(const char *name) {
	if (!IsDevBuild)
		return OS_LOG_DISABLED;
	else if (__builtin_available(macOS 10.14, *))
		return os_log_create("net.pcsx2.pcsx2", name);
	else
		return OS_LOG_DISABLED;
}

os_log_t SIGNPOST_LOGGER_EMULATION = makeOSLog("Emulation");
os_log_t SIGNPOST_LOGGER_COMPILATION = makeOSLog("Compilation");

#endif
