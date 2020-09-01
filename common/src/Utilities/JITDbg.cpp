#include "JITDbg.h"

#ifndef ENABLE_JIT_DEBUG

// If JIT debug is not enabled, stub all functions to do nothing
void JITDebugGenerator::deregisterWithID(size_t id) {}
JITDebugGenerator::JITDebugGenerator(size_t id, const char* compilerName, const char* name) {}
JITDebugGenerator::~JITDebugGenerator() {}
RegisteredFunction* JITDebugGenerator::doRegister()
{
	return nullptr;
}
JITFunctionDebugInfo JITDebugGenerator::newFunction(const char* name, size_t start, size_t end)
{
	return JITFunctionDebugInfo(nullptr, nullptr);
}
const char* JITDebugGenerator::getErrorDescription()
{
	return "Debug Generation Disabled";
}

#else

// JIT debug enabled

#include <stdio.h>
#include <string.h>
#include <algorithm>
#include <mutex>
#include <vector>

#include <libdwarf.h>
#include <dwarf.h>
#include "ELFDefs.h"

// MARK: - GDB JIT Defs

typedef enum
{
	JIT_NOACTION = 0,
	JIT_REGISTER_FN,
	JIT_UNREGISTER_FN
} jit_actions_t;

struct jit_code_entry
{
	struct jit_code_entry *next_entry;
	struct jit_code_entry *prev_entry;
	const char *symfile_addr;
	uint64_t symfile_size;
	size_t id;
	char data[];
};

struct jit_descriptor
{
	uint32_t version;
	/* This type should be jit_actions_t, but we use uint32_t
	 to be explicit about the bitwidth.  */
	uint32_t action_flag;
	struct jit_code_entry *relevant_entry;
	struct jit_code_entry *first_entry;
};

/* GDB puts a breakpoint in this function.  */
void __attribute__((noinline)) __jit_debug_register_code()
{
#ifdef __GNUC__
	__asm__ __volatile__("":::);
#endif
};

/* Make sure to specify the version statically, because the
 debugger may check the version before we can set it.  */
struct jit_descriptor __jit_debug_descriptor = { 1, 0, 0, 0 };

std::mutex jit_debug_mutex;

#ifndef _WIN32
#include <fcntl.h>
#include <unistd.h>
int jit_map_file() {
	static int fd = []() -> int {
		char name[64];
		snprintf(name, sizeof(name), "/tmp/perf-%d.map", getpid());
		return open(name, O_CREAT | O_WRONLY | O_TRUNC, 0644);
	}();
	return fd;
};
#endif

// MARK: - Dwarf Debug Info Implementation

#if defined(__x86_64__) || defined(_WIN64)
typedef ElfTypes::X64Info ElfInfo;
#else
typedef ElfTypes::I386Info ElfInfo;
#endif

// Must be called with jit_debug_mutex held
static void deregisterImpl(jit_code_entry* desc)
{
	if (desc->prev_entry)
		desc->prev_entry->next_entry = desc->next_entry;
	else
		__jit_debug_descriptor.first_entry = desc->next_entry;

	if (desc->next_entry)
		desc->next_entry->prev_entry = desc->prev_entry;

	__jit_debug_descriptor.action_flag = JIT_UNREGISTER_FN;
	__jit_debug_descriptor.relevant_entry = desc;
	__jit_debug_register_code();
	free(desc);
}

void JITDebugGenerator::deregister(RegisteredDebugInfo* fn)
{
	if (!fn)
		return;
	std::lock_guard<std::mutex> lock(jit_debug_mutex);
	deregisterImpl(reinterpret_cast<jit_code_entry*>(fn));
}

void JITDebugGenerator::deregisterAllWithID(size_t id)
{
	std::lock_guard<std::mutex> lock(jit_debug_mutex);

	for (auto desc = __jit_debug_descriptor.first_entry; desc; desc = desc->next_entry)
	{
		if (desc->id == id)
			deregisterImpl(desc);
	}
}

namespace
{
	struct StrTabData {
		std::vector<char> data;

		uint32_t add(void* in, size_t len)
		{
			size_t oldSize = data.size();
			data.resize(oldSize + len);
			memcpy(&data[oldSize], in, len);
			return (uint32_t)oldSize;
		}

		uint32_t add(const char* str)
		{
			size_t len = strlen(str);
			return add((void*)str, len + 1);
		}
	};

	struct ElfGenData
	{
		StrTabData shstrtab;
		std::vector<ElfInfo::SectionHeader> sections;

		int libdwarfCallback(
			const char*     name,
			int             size,
			Dwarf_Unsigned  type,
			Dwarf_Unsigned  flags,
			Dwarf_Unsigned  link,
			Dwarf_Unsigned  info,
			Dwarf_Unsigned* sect_name_index,
			int*            error)
		{
			int out = (int)sections.size();
			if (strcmp(name, ".rel.debug_info") == 0)
			{
				// We don't write to `sect_name_index`, which seems to cause libdwarf to not properly create a relocation table for us
				// That's fine, because we don't need one
				// But we need to skip the section for it because otherwise it'll have an entry here but not actually ever get its data assigned
				return 0;
			}

			ElfInfo::SectionHeader section = {0};
			section.name = shstrtab.add(name);
			section.type = type;
			section.flags = flags;
			section.size = size;
			section.link = link;
			section.info = info;
			section.addralign = 1;
			sections.push_back(section);
			return out;
		}
	};
}

JITDebugGenerator::JITDebugGenerator(size_t id, const char* compilerName, const char* name)
	: id(id)
{
	auto callback = [](
		const char*     name,
		int             size,
		Dwarf_Unsigned  type,
		Dwarf_Unsigned  flags,
		Dwarf_Unsigned  link,
		Dwarf_Unsigned  info,
		Dwarf_Unsigned* sect_name_index,
		void*           user_data,
		int*            error
	) -> int {
		JITDebugGenerator* dbg = static_cast<JITDebugGenerator*>(user_data);
		if (dbg->userdata == nullptr)
		{
			dbg->internalError("Libdwarf called callback before we were ready");
			return -1;
		}
		return static_cast<ElfGenData*>(dbg->userdata)->libdwarfCallback(
			name, size, type, flags, link, info, sect_name_index, error);
	};

	auto flags = ElfInfo::DwarfFlags | DW_DLC_WRITE | DW_DLC_SYMBOLIC_RELOCATIONS;
	dwarf_producer_init(flags, callback, nullptr, nullptr, this, ElfInfo::DwarfISAName, "V5", "", &dbg, &err);
	if (err) return;
	cu = dwarf_new_die(dbg, DW_TAG_compile_unit, nullptr, nullptr, nullptr, nullptr, &err);
	if (err) return;
	dwarf_add_AT_producer(cu, const_cast<char*>(compilerName), &err);
	dwarf_add_AT_name(cu, const_cast<char*>(name), &err);
}

JITDebugGenerator::~JITDebugGenerator()
{
	if (dbg)
		dwarf_producer_finish_a(dbg, &err);
}

void JITDebugGenerator::internalError(const char* desc)
{
	nonDwarfErr = desc;
	err = reinterpret_cast<Dwarf_Error_s*>(-1);
}

const char* JITDebugGenerator::getErrorDescription()
{
	if (nonDwarfErr)
		return nonDwarfErr;
	else if (err)
		return dwarf_errmsg(err);
	else
		return "No error";
}

JITFunctionDebugInfo JITDebugGenerator::newFunction(const char* name, size_t start, size_t end)
{
	if (start > end)
		internalError("Function start before end");
	if (err) return JITFunctionDebugInfo(this, nullptr);
	auto fn = dwarf_new_die(dbg, DW_TAG_subprogram, cu, nullptr, nullptr, nullptr, &err);
	if (err) return JITFunctionDebugInfo(this, nullptr);

#ifndef _WIN32
	char perfmap[128];
	int len = snprintf(perfmap, sizeof(perfmap), "%lld %lld %s\n", (uint64_t)start, (uint64_t)end, name);
	(void)write(jit_map_file(), perfmap, len);
#endif

	rangeMin = std::min(rangeMin, start);
	rangeMax = std::max(rangeMax, end);
	dwarf_add_AT_name(fn, const_cast<char*>(name), &err);
	dwarf_add_AT_targ_address(dbg, fn, DW_AT_low_pc, start, 0, &err);
	dwarf_add_AT_targ_address(dbg, fn, DW_AT_high_pc, end, 0, &err);

	return JITFunctionDebugInfo(this, fn);
}

namespace
{
	// Scoped cleanup helper for registerAll
	struct Cleanup
	{
		void** ptr;
		explicit Cleanup(void** ptr): ptr(ptr) {}
		~Cleanup() { ptr = nullptr; }
	};
}

RegisteredDebugInfo* JITDebugGenerator::doRegister()
{
	using namespace ElfTypes;

	// Add final entries to compile unit
	if (rangeMin == 0 || rangeMax == SIZE_MAX)
		internalError("Attempt to register with no functions added");
	if (err) return nullptr;
	dwarf_add_AT_targ_address(dbg, cu, DW_AT_low_pc, rangeMin, 0, &err);
	dwarf_add_AT_targ_address(dbg, cu, DW_AT_high_pc, rangeMax, 0, &err);
	dwarf_add_die_to_debug(dbg, cu, &err);
	if (err) return nullptr;

	// Create generator with base sections
	ElfGenData gen;

	ElfInfo::SectionHeader section = {0};
	section.type = SectionType::NUL;
	gen.sections.push_back(section);

	section = {0};
	section.name = gen.shstrtab.add(".text");
	section.type = SectionType::NOBITS;
	section.flags = SectionFlag::ALLOC | SectionFlag::EXECINSTR;
	section.addr = rangeMin;
	section.size = rangeMax - rangeMin;
	section.addralign = 1;
	gen.sections.push_back(section);

	auto hdr = ElfInfo::BaseHeader;

	// libdwarf requires you to give it its callback and user data on creation, but doesn't actually call it until now
	// To avoid unnecessary heap allocation, we give libdwarf a pointer to `this`, and set the actual pointer into a userdata field for the time it's actually required
	userdata = &gen;
	Cleanup deferred_cleanup(&userdata); // zero userdata before returning

	// Create section header entries
	Dwarf_Signed nsections = dwarf_transform_to_disk_form(dbg, &err);
	if (err) return nullptr;

	// Add shstrtab at the end
	hdr.shstrndx = gen.sections.size();

	section = {0};
	section.name = gen.shstrtab.add(".shstrtab");
	section.type = SectionType::STRTAB;
	section.addralign = 1;
	gen.sections.push_back(section);
	hdr.shnum = gen.sections.size();

	size_t sections_size = gen.sections.size() * sizeof(*gen.sections.data());
	size_t final_size = sizeof(ElfInfo::Header) + sections_size;
	std::vector<std::pair<void*, size_t>> dwarfsects;

	// Get section data pointers
	// Data is freed by dwarf_producer_finish in destructor
	// dwarf_get_section_bytes only works once, so we can't just get sizes and call it again later for the data
	for (Dwarf_Signed i = 0; i < nsections; i++)
	{
		Dwarf_Signed sectionIndex;
		Dwarf_Unsigned length;
		Dwarf_Ptr bytes = dwarf_get_section_bytes(dbg, i, &sectionIndex, &length, &err);
		if (sectionIndex == 0) // in case the relocation info actually does generate
			continue;
		dwarfsects.emplace_back(bytes, length);
		gen.sections[sectionIndex].offset = final_size;
		gen.sections[sectionIndex].size = length;
		final_size += length;
	}
	// Assign shstrtab size and offset
	gen.sections.back().offset = final_size;
	gen.sections.back().size = gen.shstrtab.data.size();
	final_size += gen.shstrtab.data.size();

	// Allocate entry and add data to it
	auto* new_entry = static_cast<jit_code_entry*>(malloc(sizeof(jit_code_entry) + final_size));
	new_entry->symfile_addr = new_entry->data;
	new_entry->symfile_size = final_size;
	new_entry->id = id;

	size_t cur_size = 0;
	auto add = [&](void* data, size_t size){
		memcpy(&new_entry->data[cur_size], data, size);
		cur_size += size;
	};
	add(&hdr, sizeof(hdr));
	add(gen.sections.data(), sections_size);
	for (const auto& sect : dwarfsects)
		add(sect.first, sect.second);
	add(gen.shstrtab.data.data(), gen.shstrtab.data.size());

	// Register new entry with debugger
	std::lock_guard<std::mutex> lock(jit_debug_mutex);
	new_entry->next_entry = __jit_debug_descriptor.first_entry;
	new_entry->prev_entry = nullptr;
	if (__jit_debug_descriptor.first_entry)
		__jit_debug_descriptor.first_entry->prev_entry = new_entry;
	__jit_debug_descriptor.first_entry = new_entry;
	__jit_debug_descriptor.relevant_entry = new_entry;
	__jit_debug_descriptor.action_flag = JIT_REGISTER_FN;
	__jit_debug_register_code();

	return reinterpret_cast<RegisteredDebugInfo*>(new_entry);
}

JITFunctionDebugInfo::JITFunctionDebugInfo(JITDebugGenerator* dbg, Dwarf_P_Die fn)
	: dbg(*dbg), fn(fn)
{
}

#endif
