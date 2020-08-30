cmake_minimum_required(VERSION 3.3.0...3.18)

set(VERSION 20191022)

project(Dwarf VERSION ${VERSION} LANGUAGES C)

set(PACKAGE_VERSION "\"${VERSION}\"")
set(PACKAGE_NAME "libdwarf")
set(PACKAGE_STRING "\"${PACKAGE_NAME} ${VERSION}\"")

include(CheckIncludeFile)
check_include_file( "sys/types.h"     HAVE_SYS_TYPES_H) 
check_include_file( "sys/stat.h"      HAVE_SYS_STAT_H ) 
check_include_file( "stdlib.h"        HAVE_STDLIB_H   ) 
check_include_file( "string.h"        HAVE_STRING_H   ) 
check_include_file( "memory.h"        HAVE_MEMORY_H   ) 
check_include_file( "strings.h"       HAVE_STRINGS_H  ) 
check_include_file( "stdint.h"        HAVE_STDINT_H   )
check_include_file( "unistd.h"        HAVE_UNISTD_H   )
check_include_file( "stdafx.h"        HAVE_STDAFX_H   )
check_include_file( "Windows.h"       HAVE_WINDOWS_H  )
check_include_file( "alloca.h"        HAVE_ALLOCA_H   )
# PCSX2 doesn't need these
# check_include_file( "sgidefs.h"       HAVE_SGIDEFS_H  )
# check_include_file( "elf.h"           HAVE_ELF_H      ) 
# check_include_file( "libelf.h"        HAVE_LIBELF_H   ) 
# check_include_file( "libelf/libelf.h" HAVE_LIBELF_LIBELF_H) 
# check_include_file( "elfaccess.h"     HAVE_ELFACCESS_H)
# check_include_file( "sys/elf_386.h"   HAVE_SYS_ELF_386_H  )
# check_include_file( "sys/elf_amd64.h" HAVE_SYS_ELF_AMD64_H)
# check_include_file( "sys/elf_sparc.h" HAVE_SYS_ELF_SPARC_H)
# check_include_file( "sys/ia64/elf.h"  HAVE_SYS_IA64_ELF_H )

add_subdirectory(src)