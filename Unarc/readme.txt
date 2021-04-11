SFX modules and tiny decompressor for FreeArc (http://freearc.org)

SFX modules, classification by OS/subsystem:
    arc*.sfx.linux - console Linux-i386 self-extractors
    freearc*.sfx   - GUI Win32 self-extractors
    arc*.sfx       - console Win32 self-extractors

SFX modules, classification by included compression methods:
    *arc.sfx      - full, includes decomression code for all FreeArc compression/encryption algorithms and external compression DLLs (CLS)
    *arc-mini.sfx - without encryption/mm/tta/grzip/tornado/dispack/lz4, i.e. for archives created with options like -mx -mm-
    *arc-tiny.sfx - also without rep/lzp/ppmd, i.e. for archives created with options like -m9x -mm-

Modules for producing simple installers:
    freearc-installer.sfx - extracts files into temporary directory, runs setup.exe and deletes extracted files when it finished
    freearc-installer-nodelete.sfx - extracts files into temporary directory and runs setup.exe

Standalone extractors of FreeArc archives:
    unarc.exe - console Win32 extractor
    unarc     - console Linux-i386 extractor

DLL for decompression of FreeArc archives in 3rd-party software:
    unarc.dll - provides FreeArcExtract() function, examples of its usage are provided in the Addons directory
