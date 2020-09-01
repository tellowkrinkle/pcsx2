#pragma once

#include <stdint.h>
#include <stddef.h>

class JITFunctionDebugInfo;
struct RegisteredDebugInfo;
typedef struct Dwarf_P_Debug_s* Dwarf_P_Debug;
typedef struct Dwarf_P_Die_s*   Dwarf_P_Die;
typedef struct Dwarf_Error_s*   Dwarf_Error;

class JITDebugGenerator
{
	friend class JITFunctionDebugInfo;
	size_t id;
	Dwarf_P_Debug dbg = nullptr;
	Dwarf_P_Die cu = nullptr;
	Dwarf_Error err = nullptr;
	const char* nonDwarfErr = nullptr;
	size_t rangeMin = SIZE_MAX;
	size_t rangeMax = 0;
	void* userdata = nullptr;

	// Can't copy or move due to libdwarf taking a pointer to it in initialization
	JITDebugGenerator(const JITDebugGenerator&) = delete;
	JITDebugGenerator(JITDebugGenerator&&) = delete;
	void internalError(const char* desc);

public:
	/// Deregister the given function
	static void deregister(RegisteredDebugInfo* fn);

	/// Deregister all functions with the given ID
	static void deregisterAllWithID(size_t id);

	/// Create a debug info generator with the given information
	JITDebugGenerator(size_t id, const char* compilerName, const char* name);
	~JITDebugGenerator();

	/// Create debug info for a new function
	///
	/// @param start The first memory address of the function's code
	/// @param end One past the last memory address of the function's code
	/// @note You can register multiple functions but the range containing all the functions should not overlap with that of other JITDebugGenerators
	JITFunctionDebugInfo newFunction(const char* name, size_t start, size_t end);

	/// Register all functions
	/// @warning Once you call this, you may not use `newFunction`
	RegisteredDebugInfo* doRegister();

	/// Returns false if any errors have occurred
	bool isOkay() { return err == nullptr; }

	/// Returns a string containing a description of the error that has ocurred
	const char* getErrorDescription();
};

class JITFunctionDebugInfo
{
	friend class JITDebugGenerator;
	JITDebugGenerator& dbg;
	Dwarf_P_Die fn;

	JITFunctionDebugInfo(JITDebugGenerator* dbg, Dwarf_P_Die fn);
public:
	
};
