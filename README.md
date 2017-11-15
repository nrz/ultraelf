# ultraELF multiarchitecture assembler, disassembler and metamorphic engine.
A hobby project of mine. Work in progress.

## License
ultraELF is under The GNU General Public License 3,
or (at your option) any later version. See COPYING file.
All my code copyright (C) 2016-2017 Antti Nuortimo.

## Compiling
ultraELF compiles at least with SBCL (tested with SBCL 1.2.4.debian).
For compiling and loading instructions please see file `ultraelf.asd`.
For manual please see file `manual.txt`.

All tests pass. So you should receive a message "No tests failed." when
running `(test-parser)` and also `(test-x64-assembling-functions)`
following the instructions in ultraelf.asd .

## Implemented functionality
* Binary code generation for quite many x86-64 instruction encodings.
* Generation of all different functionally equivalent encodings for quite many x86-64 instructions.
* Using Common Lisp as a macro language in ultraELF assembler.

## Future developments underway:
* Code generation for all x86-64 instruction encodings.
* Writing of executable files.
* Disassembler.
* Metamorphic engine.
* Steganographic assembler.
* Support for x86-16.
* Support for x86-32.
* Support for ARM.
* Support for other CPU architectures.
* Porting to C++ (with TinyScheme as a macro language).

## Contact info
(found bugs and suggestions are very welcome!)

| variable       | value                                                        |
|:-------------- |:------------------------------------------------------------ |
| name           | Antti Nuortimo                                               |
| GitHub         | https://github.com/nrz                                       |
| Stack Overflow | http://stackoverflow.com/users/1310991/nrz                   |
| email          | antti dot nuortimo at gmail dot com                          |
|                | (please write 'ultraELF' to email title to escape /dev/null) |

Work offers are also very welcome!
