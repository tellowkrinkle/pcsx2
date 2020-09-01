#pragma once

#include <stdint.h>
#include <dwarf.h>

namespace ElfTypes
{
	namespace Endian
	{
		enum Value: uint8_t { NONE = 0, LITTLE, BIG };
	}

	namespace Class
	{
		enum Value: uint8_t { NONE = 0, C32, C64 };
	}

	namespace Machine
	{
		enum Value: uint16_t
		{
			NONE = 0,
			I386 = 3,
			X86_64 = 62,
		};
	}

	namespace Type
	{
		enum Value: uint16_t { NONE = 0, REL, EXEC, DYN, CORE };
	}

	namespace SectionType
	{
		enum Value: uint32_t
		{
			NUL = 0, PROGBITS, SYMTAB, STRTAB, RELA, HASH, DYNAMIC, NOTE, NOBITS, REL, SHLIB, DYNSYM,
			INIT_ARRAY = 14, FINI_ARRAY, PREINIT_ARRAY, GROUP, SYMTAB_SHNDX,
		};
		static_assert(DYNSYM == 11, "Sanity Check");
		static_assert(SYMTAB_SHNDX == 18, "Sanity Check");
	}

	namespace SectionFlag
	{
		enum Value: uint32_t
		{
			WRITE = 0x1,
			ALLOC = 0x2,
			EXECINSTR = 0x4,
			MERGE = 0x10,
			STRINGS = 0x20,
			INFO_LINK = 0x40,
			LINK_ORDER = 0x80,
			OS_NONCONFORMING = 0x100,
			GROUP = 0x200,
			TLS = 0x400,
			MASKOS = 0x0ff00000,
			MASKPROC = 0xf0000000,
		};
	}

	template <typename IntPtr>
	struct Header
	{
		uint8_t  ei_magic[4];
		uint8_t  ei_class;
		uint8_t  ei_endian;
		uint8_t  ei_version;
		uint8_t  ei_osabi;
		uint8_t  ei_abiversion;
		uint8_t  ei_pad[7];
		uint16_t type;
		uint16_t machine;
		uint32_t version;
		IntPtr   entry;
		IntPtr   phoff;
		IntPtr   shoff;
		uint32_t flags;
		uint16_t ehsize;
		uint16_t phentsize;
		uint16_t phnum;
		uint16_t shentsize;
		uint16_t shnum;
		uint16_t shstrndx;
	};
	static_assert(sizeof(Header<uint32_t>) == 52, "Header size check");
	static_assert(sizeof(Header<uint64_t>) == 64, "Header size check");

	template <typename IntPtr>
	struct SectionHeader
	{
		uint32_t name;
		uint32_t type;
		IntPtr   flags;
		IntPtr   addr;
		IntPtr   offset;
		IntPtr   size;
		uint32_t link;
		uint32_t info;
		IntPtr   addralign;
		IntPtr   entsize;
	};
	static_assert(sizeof(SectionHeader<uint32_t>) == 40, "Header size check");
	static_assert(sizeof(SectionHeader<uint64_t>) == 64, "Header size check");

	template <typename IntPtr>
	static constexpr Header<IntPtr> MakeBaseHeader(Endian::Value endian, Machine::Value machine)
	{
		return (Header<IntPtr>){
			.ei_magic = { 0x7f, 'E', 'L', 'F' },
			.ei_class = sizeof(IntPtr) == 4 ? Class::C32 : Class::C64,
			.ei_endian = endian,
			.ei_version = 1,
			.ei_osabi = 0,
			.ei_abiversion = 0,
			.ei_pad = {0},
			.type = Type::REL,
			.machine = machine,
			.version = 1,
			.entry = 0,
			.phoff = 0,
			.shoff = sizeof(Header<IntPtr>),
			.flags = 0,
			.ehsize = sizeof(Header<IntPtr>),
			.phentsize = 0,
			.phnum = 0,
			.shentsize = sizeof(SectionHeader<IntPtr>),
			.shnum = 0,
			.shstrndx = 0,
		};
	}

	struct I386Info
	{
		typedef ElfTypes::Header<uint32_t> Header;
		typedef ElfTypes::SectionHeader<uint32_t> SectionHeader;
		static constexpr Header BaseHeader = MakeBaseHeader<uint32_t>(Endian::LITTLE, Machine::I386);
		static constexpr Dwarf_Unsigned DwarfFlags = DW_DLC_SIZE_32 | DW_DLC_TARGET_LITTLEENDIAN;
		static constexpr const char* DwarfISAName = "i386";
	};

	struct X64Info
	{
		typedef ElfTypes::Header<uint64_t> Header;
		typedef ElfTypes::SectionHeader<uint64_t> SectionHeader;
		static constexpr Header BaseHeader = MakeBaseHeader<uint64_t>(Endian::LITTLE, Machine::X86_64);
		static constexpr Dwarf_Unsigned DwarfFlags = DW_DLC_SIZE_64 | DW_DLC_TARGET_LITTLEENDIAN;
		static constexpr const char* DwarfISAName = "x86_64";
	};
}
