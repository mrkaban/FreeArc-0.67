How to compile FreeArc:

== ON WINDOWS ===========================================================================

1. Download GHC 6.10.3 bundled with C++ compiler:
     http://www.haskell.org/ghc/dist/6.10.3/ghc-6.10.3-i386-windows.exe
2. Install it into C:\Base\Compiler\ghc directory
3. Make sure that make.exe is available via your PATH. If you yet don't have make,
     download http://sourceforge.net/project/showfiles.php?group_id=2435
     and rename mingw32-make.exe to make.exe
4. Install Gtk2Hs: http://sunet.dl.sourceforge.net/project/gtk2hs/gtk2hs/0.10.1/gtk2hs-0.10.1-win32-installer.exe
5. Extract FreeArc sources into C:\!\FreeArchiver directory
6. Extract 7-zip 9.10 sources into C:\!\FreeArchiver\7zip directory:
     https://sourceforge.net/projects/sevenzip/files/7-Zip/9.10%20beta/7z910.tar.bz2/download
7. Compile HsLua:
     cd HsLua
     make
     cd ..
8. Compile SFX modules, Unarc.exe, Unarc.dll and FAR plugin (and icon.o required for building Arc.exe/FreeArc.exe):
     cd Unarc
     make
9. Compile console version (Arc.exe will be placed to Tests directory):
     compile-O2.cmd
10. Compile GUI version (FreeArc.exe will be placed to Tests directory):
     compile-GUI-O2.cmd
11. Optionally - compile facompress.dll and facompress_mt.dll (requires Intel C++ compiler):
      cd Compression
      compile-dll.cmd
12. Optionally - compile ArcShellExt.dll/ArcShellExt-64.dll/all2arc.exe/Manager*.exe:
      cd ArcShellExt
      compile-32.cmd
      compile-64.cmd
      cd all2arc
      make
      cd ..
      cd Register
      compile.cmd
      compile64.cmd


== ON UNIX (tested on Fedora 12) ========================================================

1. Install GNU make, gcc, gcc c++, curl-devel, ncurses-devel, GHC 6.10.4 (http://haskell.org/ghc/download_ghc_6_10_4.html),
     Gtk2Hs 0.10.1 (http://darcs.haskell.org/gtk2hs/download).
   Package names for Fedora 12 are: make, gcc, gcc-c++, ghc, ghc-gtk2hs, curl-devel, ncurses-devel
2. Extract sources to some directory and go to this directory. Execute command:
     chmod +x compile*
3. Extract p7zip 9.04 sources into 7zip subdirectory of this directory:
     https://sourceforge.net/projects/p7zip/files/p7zip/9.04/p7zip_9.04_src_all.tar.bz2/download
4. Compile and install HsLua:
     cd HsLua && sudo make && cd ..
5. Compile console version (arc will be placed to Tests directory):
     ./compile-O2
6. Compile GUI version (freearc will be placed to Tests directory):
     ./compile-GUI-O2
7. Compile SFX modules and Unarc:
     cd Unarc
     make unix
