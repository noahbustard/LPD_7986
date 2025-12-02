# Simulation Description: MOV AL, imm

## Instruction Encoding
- **Instruction Format**: MOV R,imm → `1011 0 reg immdata`
- **Register Selected**: AL (reg field = 000)
- **Immediate Value**: 0x3C
- **Encoded Instruction**: 1011 0000 0011 1100 → **0xB03C**

## Test Vector Timeline

| Time | Event | Details |
|------|-------|---------|
| 0 ns | Initial State | All signals at zero |
| 100 ns | Load Instruction | instruction[15..0] = 0xB03C (1011 0000 0011 1100) |
| 120 ns | Execute Edge | Rising edge on `exe` → instruction register latches value |
| 200 ns | Update Edge | Rising edge on `upd` → AL register loads ALU output |

## Execution Details
- **ALU Configuration**: Pass immediate value straight through
- **Result**: AL = 0x3C after `upd` edge

## Output Verification
- **Y[7..0] Bus Output**: 0011 1100 (0x3C) ✓
- **Seven-Segment Display**: Signals a0..a6 and b0..b6 match hex-decoder patterns for "3C" ✓

## Functional Verification
All datapath components working correctly:
- ✓ Instruction encoding
- ✓ Control logic (opcode decoding)
- ✓ Register file
- ✓ ALU
- ✓ Hex display path
