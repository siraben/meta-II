# Bytecode Machine Specifications

## Opcode specification
_Documentation for the bytecode instructions can be found in the
original paper._


Each opcode takes up a byte corresponding to the following table:

- `<STR>` indicates a null-terminated ASCII string.
- `<ADDR>` indicates a 16-bit unsigned little endian address.

| Hex Opcode | Instruction  | Name                                                     |
| :-:        | :-:          | :-:                                                      |
| 0          | `TST <STR>`  | Test for `<STR>`.                                        |
| 1          | `ID`         | Read identifier to token buffer.                         |
| 2          | `NUM`        | Read number to token buffer.                             |
| 3          | `SR`         | Read single-quote delimited string to token buffer.      |
| 4          | `CLL <ADDR>` | Call subroutine.                                         |
| 5          | `R`          | Return from subroutine.                                  |
| 6          | `SET`        | Set switch to true.                                      |
| 7          | `B <ADDR>`   | Unconditional branch.                                    |
| 8          | `BT <ADDR>`  | Branch if switch is true.                                |
| 9          | `BF <ADDR>`  | Branch if switch is false.                               |
| A          | `BE`         | Error if switch is false.                                |
| B          | `CL <STR>`   | Copy `<STR>` to output buffer.                           |
| C          | `CI`         | Copy token buffer to output buffer.                      |
| D          | `GN1`        | Generate unique label if L1 is blank, then output label. |
| E          | `GN2`        | Generate unique label if L2 is blank, then output label. |
| F          | `LB`         | Set output buffer to first column.                       |
| 10         | `OUT`        | Output line terminator, set output column to 8th column. |
| 11         | `END`        | Halt immediately.                                        |

Where `<ADDR>` is indicated, this is a 16-bit address (little endian,
or lowest byte first).  So the instruction `B $1234` becomes `$7 $34
$12`.

## Flags
There is a single flag known in the paper as the "switch".  This flag
can be set or reset.  The following opcodes are the only ones that may
modify the switch: `TST <STR>`, `ID`, `NUM`, `SR`, `SET`.  The
following opcodes are the only ones whose behavior depends on the
switch: `BT <ADDR>`, `BF <ADDR>`, `BE <ADDR>`.

## Registers
A 16-bit program counter (PC) is used to keep track of the current
execution state.  This number is initialized to 0 at program start.
It is incremented after every instruction.  It may be modified
directly by the opcode `B`.

There is a pointer to the input buffer, but its value cannot be
accessed directly.  It is modified by any opcode that reads from the
input buffer (`TST`, `ID`, `NUM`, `SR`).

There is a pointer to the output buffer, but its value cannot be
accessed directly.  It is modified by any opcode that outputs to the
output buffer (`GN1`, `GN2`, `LB`, `OUT`, `CL`, `CI`).

## Stack
The only opcode that can push to the stack is `CLL <ADDR>`, which
pushes a stackframe with the following three contents:

| Size (bits) | Name           | Initialization      |
| :-:         | :-:            | :-:                 |
| 8           | Label 1        | 0                   |
| 8           | Label 2        | 0                   |
| 16          | Return address | Program counter + 1 |

Label 1 (L1) and Label 2 (L2) may be modified by calls to `GN1` and
`GN2` respectively.  On execution of `R` the topmost stackframe is
popped.

## Memory regions
The META II virtual machine has two memory regions: the token buffer
and input buffer.  The token buffer can be of any size.  The recommend
length for the token buffer is 256 bytes (ASCII characters).  The
recommended length for the input buffer is 65536 bytes (ASCII
characters).

## Error handling
A META II bytecode runner should halt with the appropriate error
message when encountering the following error conditions:

### Invalid opcode
When the machine is not executing an opcode and the instruction found
at the memory location of PC is larger than the largest opcode (17).

### Address out of bounds
When the `<ADDR>` argument of opcodes `CLL`, `B`, `BT`, `BF`, or `BE`
point at an invalid memory location.  Detecting such a invalid memory
access is implementation dependent.


