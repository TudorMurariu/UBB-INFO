ALINK v1.6 is copyright 1998/9 Anthony A.J. Williams. All Rights Reserved.

ALINK is a linker for OBJ and LIB files, and generates MSDOS COM and 
MSDOS EXE files, and Win32 PE files. Win32 resource files are also supported
for linking to PE files. MS/Intel OMF and MS-COFF object and library files
are supported. MS-COFF import libraries are not supported, and will cause
an undefined symbol error in a .idata section.

ALINK is free, and as such there is no warranty whatsoever. If anything bad
happens, it is entirely your responsibility.

The command line is:

ALINK [options] filename [options] [filenames] [options] ...

By default ALINK performs a case-sensitive link to an EXE file, without
padding segments. If filenames don't include an extension, .obj is assumed.
Additional parameters can be taken from response files, by specifying a 
filename preceded by an @ sign, e.g.

ALINK @response.fil

Blanks lines are ignored, options may be specified together, or on separate
lines. Quoted strings are treated as a single parameter.
Semicolons are treated as the start of a comment, and the rest of the
line will be ignored. If you really want a semicolon, enclose it in
quotes.
Options and filenames can be used in the parameter file, but filenames may 
not include wildcards. Multiple response files can be referenced, and 
additional response files may be included from within a response file. 
Cyclic references are not detected, and will cause the program
to loop until it runs out of memory (never, if the response files don't
contain any options!).

Possible options are:

-c      Enable Case sensitive link
-c+     "
-c-     Disable Case sensitive link
-p      Pad segments (initialise all segments)
-p+     "
-p-     Disable segment padding
-o yyy  yyy is output filename
-oxxx   xxx specifies output format
        COM = output COM file
        EXE = output EXE file
        PE  = output Win32 PE file (.EXE)
-m      Generate map file
-m+     "
-m-     Don't generate map file
-L ddd  Add directory ddd to library search list
-h      Display this help list
-H      "
-?      "
-entry name  Use public symbol name as the entry point

In addition, for PE files the following options apply:

-base addr           Set Image Base (default=4Mb)
-objectalign xxx     Set section alignment in memory (default=64K)
-filealign xxx       Set section alignment in file (default=512)
-subsys xxx          Set windows subsystem to use (default=windows)
                     windows, win or gui => windows subsystem
                     console, con or char => console subsystem
                     native => native subsystem
                     posix => POSIX subsystem
-subsysver x.y       Set subsystem version x.y (default=4.0)
-osver x.y           Set OS version x.y (default=1.0)
-heapsize xxx        Set heap size (default=1Mb)
-heapcommitsize xxx  Set heap commit size (default=4K)
-stacksize xxx       Set stack size (default=1Mb)
-stackcommitsize xxx Set stack commit size (default=4K)
-dll                 Build a DLL instead of an EXE file.
-stub filename       Use the specified file as the MSDOS stub.

Note that Windows 3.1=> ver 3.10, 3.5=>ver 3.50
If you type -subsysver 3.1, this is the same as 3.01

The alignments must be powers of 2. ObjectAlign can be 512 to 256Mb, FileAlign can
be 512 to 64K. The Image base and alignments can be specified as decimal numbers, or as
hex numbers prefixed by 0x (in fact, any number format supported by the C function
strtoul). The image base must be a multiple of 64K.

options are case sensitive, so -ocom is invalid, as is -C.

If an output filename is not specified, then the output file is determined
from the output type, and the root of the first object file.

e.g.

        ALINK file1.obj -oCOM -o outfile.com

        Generates outfile.com from file1.obj

        ALINK file2.obj file3.obj -oEXE

        Generates file2.exe

Segments from different object files are combined if they have the same name
and class, and are not declared private. Absolute segments are never
combined, and stack segments are always combined.

Segments are emitted in the order they are present in the object files,
unless groups are used. All segments in groups are emitted before segments
not in groups. A segment may not be in more than one group. Segments in groups
are emitted in the order specified in the group definition. If two different
definitions of the same group specify different orders, the first definition
is used.

Thus, if in the second example above, file2.obj contains a group definition
equivalent to

        agroup group code,data,stack

and file3.obj contains a group definition equivalent to

        agroup group data,stack,bss,code

The final output for agroup will be

        code,data,stack,bss

The text output from the linker is quite verbose, and lists all the files as
they are loaded in. Any unrecognised object records will cause an error. Any
recognised, but ignored records, such as LINNUM records and COMENT records
will be printed on the screen. Since debug information is often contained in
COMENT records, this could lead to a lot of output.

The map file will list the segments, publics, imports and exports.

A sample Import library for Win32 is included as win32.lib. All named exports in
Kernel32, User32, GDI32, Shell32, ADVAPI32, version,winmm, lz32,commdlg and commctl
are included. Use

alink -oPE file[.obj] win32.lib to include it.

or specify

INCLUDELIB "win32"

in your source file (TASM)

This consists of a series of entries for import redirection - call
MessageBoxA, and it jumps to [__imp_MessageBoxA], which is in the Import
Table.

Thus calls to imports will run faster if call [__imp_importName] is used
instead of call importName.

See test.asm, my sample program, which calls up a message box both ways.

See rtn.asm for an example program using NASM, and resources.

Any questions, comments or bug reports please email me at

anthony_w@geocities.com
