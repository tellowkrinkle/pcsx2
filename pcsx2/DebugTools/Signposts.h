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

// Signposts - Defines markers for use with debugging tools

#pragma once

namespace signposts {
	namespace enabled {
		constexpr bool EEInt = true;
		constexpr bool EERec = true;
		constexpr bool IOPInt = false;
		constexpr bool IOPRec = false;
		constexpr bool VU0Int = true;
		constexpr bool VU1Int = true;
		constexpr bool VU0Rec = true;
		constexpr bool VU1Rec = true;
		constexpr bool EERecCompile = true;
		constexpr bool IOPRecCompile = true;
		constexpr bool VU0RecCompile = true;
		constexpr bool VU1RecCompile = true;
		constexpr bool VIFTransfer = false;
		constexpr bool GIFTransfer = false;
	}
}

#ifdef __APPLE__
# include <AvailabilityVersions.h>
# define SIGNPOST_USE_OSLOG 1
# if SIGNPOST_USE_OSLOG && defined(__MAC_10_14)
#  include <os/signpost.h>
#  undef FSCALE

extern os_log_t SIGNPOST_LOGGER_EMULATION;
extern os_log_t SIGNPOST_LOGGER_COMPILATION;

namespace signposts {
	namespace loggers {
		constexpr os_log_t& EEInt = SIGNPOST_LOGGER_EMULATION;
		constexpr os_log_t& EERec = SIGNPOST_LOGGER_EMULATION;
		constexpr os_log_t& IOPInt = SIGNPOST_LOGGER_EMULATION;
		constexpr os_log_t& IOPRec = SIGNPOST_LOGGER_EMULATION;
		constexpr os_log_t& VU0Int = SIGNPOST_LOGGER_EMULATION;
		constexpr os_log_t& VU1Int = SIGNPOST_LOGGER_EMULATION;
		constexpr os_log_t& VU0Rec = SIGNPOST_LOGGER_EMULATION;
		constexpr os_log_t& VU1Rec = SIGNPOST_LOGGER_EMULATION;
		constexpr os_log_t& EERecCompile = SIGNPOST_LOGGER_COMPILATION;
		constexpr os_log_t& IOPRecCompile = SIGNPOST_LOGGER_COMPILATION;
		constexpr os_log_t& VU0RecCompile = SIGNPOST_LOGGER_COMPILATION;
		constexpr os_log_t& VU1RecCompile = SIGNPOST_LOGGER_COMPILATION;
		constexpr os_log_t& VIFTransfer = SIGNPOST_LOGGER_EMULATION;
		constexpr os_log_t& GIFTransfer = SIGNPOST_LOGGER_EMULATION;
	}
}

// These must be string literals

#  define SIGNPOST_NAME_EEInt "EE Interpreter"
#  define SIGNPOST_NAME_EERec "EE Rec"
#  define SIGNPOST_NAME_IOPInt "IOP Interpreter"
#  define SIGNPOST_NAME_IOPRec "IOP Rec"
#  define SIGNPOST_NAME_VU0Int "VU0 Interpreter"
#  define SIGNPOST_NAME_VU1Int "VU1 Interpreter"
#  define SIGNPOST_NAME_VU0Rec "VU0 Rec"
#  define SIGNPOST_NAME_VU1Rec "vu1 Rec"
#  define SIGNPOST_NAME_EERecCompile "EE Rec Compile"
#  define SIGNPOST_NAME_IOPRecCompile "IOP Rec Compile"
#  define SIGNPOST_NAME_VU0RecCompile "VU0 Rec Compile"
#  define SIGNPOST_NAME_VU1RecCompile "VU1 Rec Compile"
#  define SIGNPOST_NAME_VIFTransfer "VIF Transfer"
#  define SIGNPOST_NAME_GIFTransfer "GIF Transfer"

#  define SIGNPOST_FORMAT_EEInt
#  define SIGNPOST_FORMAT_EERec
#  define SIGNPOST_FORMAT_IOPInt
#  define SIGNPOST_FORMAT_IOPRec
#  define SIGNPOST_FORMAT_VU0Int
#  define SIGNPOST_FORMAT_VU1Int
#  define SIGNPOST_FORMAT_VU0Rec
#  define SIGNPOST_FORMAT_VU1Rec
#  define SIGNPOST_FORMAT_EERecCompile "@%08x",
#  define SIGNPOST_FORMAT_IOPRecCompile "@%08x",
#  define SIGNPOST_FORMAT_VU0RecCompile "@%08x",
#  define SIGNPOST_FORMAT_VU1RecCompile "@%08x",
#  define SIGNPOST_FORMAT_VIFTransfer "VU%d",
#  define SIGNPOST_FORMAT_GIFTransfer "Path %d",

#  define SIGNPOST_START_IMPL(subsystem, ...) if (__builtin_available(macOS 10.14, *)) \
	os_signpost_interval_begin(signposts::loggers::subsystem, OS_SIGNPOST_ID_EXCLUSIVE, SIGNPOST_NAME_##subsystem, SIGNPOST_FORMAT_##subsystem __VA_ARGS__)
#  define SIGNPOST_END_IMPL(subsystem, ...) if (__builtin_available(macOS 10.14, *)) \
	os_signpost_interval_end(signposts::loggers::subsystem, OS_SIGNPOST_ID_EXCLUSIVE, SIGNPOST_NAME_##subsystem)
# elif defined(__MAC_10_12)
#   include <sys/kdebug_signpost.h>

namespace signposts {
	namespace ids {
		constexpr int EEInt = 1;
		constexpr int EERec = 1;
		constexpr int IOPInt = 2;
		constexpr int IOPRec = 2;
		constexpr int VU0Int = 3;
		constexpr int VU1Int = 3;
		constexpr int VU0Rec = 3;
		constexpr int VU1Rec = 3;
		constexpr int EERecCompile = 4;
		constexpr int IOPRecCompile = 5;
		constexpr int VU0RecCompile = 6;
		constexpr int VU1RecCompile = 6;
		constexpr int VIFTransfer = 7;
		constexpr int GIFTransfer = 8;
	}
	namespace colors {
		constexpr int EEInt = 0;
		constexpr int EERec = 2;
		constexpr int IOPInt = 0;
		constexpr int IOPRec = 2;
		constexpr int VU0Int = 0;
		constexpr int VU1Int = 1;
		constexpr int VU0Rec = 2;
		constexpr int VU1Rec = 3;
		constexpr int EERecCompile = 2;
		constexpr int IOPRecCompile = 2;
		constexpr int VU0RecCompile = 2;
		constexpr int VU1RecCompile = 3;
		constexpr int VIFTransfer = -1;
		constexpr int GIFTransfer = -1;
	}
}

static void kdebug_signpost_start_helper(uint32_t subsystem, uintptr_t color, uintptr_t data1 = 0, uintptr_t data2 = 0, uintptr_t data3 = 0)
{
	if (!IsDevBuild) return;
	if (__builtin_available(macOS 10.12, *)) {
		if (color == (uintptr_t)-1)
			color = data1;
		kdebug_signpost_start(subsystem, data1, data2, data3, color);
	}
}

static void kdebug_signpost_end_helper(uint32_t subsystem, uintptr_t color, uintptr_t data1 = 0, uintptr_t data2 = 0, uintptr_t data3 = 0)
{
	if (!IsDevBuild) return;
	if (__builtin_available(macOS 10.12, *)) {
		if (color == (uintptr_t)-1)
			color = data1;
		kdebug_signpost_end(subsystem, data1, data2, data3, color);
	}
}

#  define SIGNPOST_START_IMPL(subsystem, ...) kdebug_signpost_start_helper(signposts::ids::subsystem, signposts::colors::subsystem, ##__VA_ARGS__)
#  define SIGNPOST_END_IMPL(subsystem, ...) kdebug_signpost_end_helper(signposts::ids::subsystem, signposts::colors::subsystem, ##__VA_ARGS__)
# endif
#endif

#ifndef SIGNPOST_START_IMPL

#define SIGNPOST_START_IMPL(subsystem, ...)
#define SIGNPOST_END_IMPL(subsystem, ...)

#endif

#define SIGNPOST_START(subsystem, ...) if (signposts::enabled::subsystem) \
	SIGNPOST_START_IMPL(subsystem, ##__VA_ARGS__)
#define SIGNPOST_END(subsystem, ...) if (signposts::enabled::subsystem) \
	SIGNPOST_END_IMPL(subsystem, ##__VA_ARGS__)
