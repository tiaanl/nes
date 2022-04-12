use crate::cpu::{Cpu, Flags};
use crate::{Bus, Offset, Operand};
use std::ops::{BitAnd, BitOr, BitXor};

impl<B: Bus> Cpu<B> {
    #[rustfmt::skip]
    pub const MAP: [(fn(&mut Cpu<B>, &Operand), fn(&mut Cpu<B>) -> Operand, &'static str, u8, bool); 256] = [
        /* 00 */ (Cpu::op_brk, Cpu::am_imp, "BRK", 7, true ), // BRK  impl
        /* 01 */ (Cpu::op_ora, Cpu::am_izx, "ORA", 6, true ), // ORA  X,ind
        /* 02 */ (Cpu::op_jam, Cpu::am_imp, "JAM", 2, false), // JAM
        /* 03 */ (Cpu::op_slo, Cpu::am_izx, "SLO", 8, false), // SLO  X,ind
        /* 04 */ (Cpu::op_nop, Cpu::am_zp0, "NOP", 3, false), // NOP  zpg
        /* 05 */ (Cpu::op_ora, Cpu::am_zp0, "ORA", 3, true ), // ORA  zpg
        /* 06 */ (Cpu::op_asl, Cpu::am_zp0, "ASL", 5, true ), // ASL  zpg
        /* 07 */ (Cpu::op_slo, Cpu::am_zp0, "SLO", 5, false), // SLO  zpg
        /* 08 */ (Cpu::op_php, Cpu::am_imp, "PHP", 3, true ), // PHP  impl
        /* 09 */ (Cpu::op_ora, Cpu::am_imm, "ORA", 2, true ), // ORA  #
        /* 0A */ (Cpu::op_asl, Cpu::am_imp, "ASL", 2, true ), // ASL  A
        /* 0B */ (Cpu::op_anc, Cpu::am_imm, "ANC", 2, false), // ANC  #
        /* 0C */ (Cpu::op_nop, Cpu::am_abs, "NOP", 4, false), // NOP  abs
        /* 0D */ (Cpu::op_ora, Cpu::am_abs, "ORA", 4, true ), // ORA  abs
        /* 0E */ (Cpu::op_asl, Cpu::am_abs, "ASL", 6, true ), // ASL  abs
        /* 0F */ (Cpu::op_slo, Cpu::am_abs, "SLO", 6, false), // SLO  abs
        /* 10 */ (Cpu::op_bpl, Cpu::am_rel, "BPL", 2, true ), // BPL  rel
        /* 11 */ (Cpu::op_ora, Cpu::am_izy, "ORA", 5, true ), // ORA  ind,Y
        /* 12 */ (Cpu::op_jam, Cpu::am_imp, "JAM", 2, false), // JAM
        /* 13 */ (Cpu::op_slo, Cpu::am_izy, "SLO", 8, false), // SLO  ind,Y
        /* 14 */ (Cpu::op_nop, Cpu::am_zpx, "NOP", 4, false), // NOP  zpg,X
        /* 15 */ (Cpu::op_ora, Cpu::am_zpx, "ORA", 4, true ), // ORA  zpg,X
        /* 16 */ (Cpu::op_asl, Cpu::am_zpx, "ASL", 6, true ), // ASL  zpg,X
        /* 17 */ (Cpu::op_slo, Cpu::am_zpx, "SLO", 6, false), // SLO  zpg,X
        /* 18 */ (Cpu::op_clc, Cpu::am_imp, "CLC", 2, true ), // CLC  impl
        /* 19 */ (Cpu::op_ora, Cpu::am_aby, "ORA", 4, true ), // ORA  abs,Y
        /* 1A */ (Cpu::op_nop, Cpu::am_imp, "NOP", 2, false), // NOP  impl
        /* 1B */ (Cpu::op_slo, Cpu::am_aby, "SLO", 7, false), // SLO  abs,Y
        /* 1C */ (Cpu::op_nop, Cpu::am_abx, "NOP", 4, false), // NOP  abs,X
        /* 1D */ (Cpu::op_ora, Cpu::am_abx, "ORA", 4, true ), // ORA  abs,X
        /* 1E */ (Cpu::op_asl, Cpu::am_abx, "ASL", 7, true ), // ASL  abs,X
        /* 1F */ (Cpu::op_slo, Cpu::am_abx, "SLO", 7, false), // SLO  abs,X
        /* 20 */ (Cpu::op_jsr, Cpu::am_abs, "JSR", 6, true ), // JSR  abs
        /* 21 */ (Cpu::op_and, Cpu::am_izx, "AND", 6, true ), // AND  X,ind
        /* 22 */ (Cpu::op_jam, Cpu::am_imp, "JAM", 2, false), // JAM
        /* 23 */ (Cpu::op_rla, Cpu::am_izx, "RLA", 8, false), // RLA  X,ind
        /* 24 */ (Cpu::op_bit, Cpu::am_zp0, "BIT", 3, true ), // BIT  zpg
        /* 25 */ (Cpu::op_and, Cpu::am_zp0, "AND", 3, true ), // AND  zpg
        /* 26 */ (Cpu::op_rol, Cpu::am_zp0, "ROL", 5, true ), // ROL  zpg
        /* 27 */ (Cpu::op_rla, Cpu::am_zp0, "RLA", 5, false), // RLA  zpg
        /* 28 */ (Cpu::op_plp, Cpu::am_imp, "PLP", 4, true ), // PLP  impl
        /* 29 */ (Cpu::op_and, Cpu::am_imm, "AND", 2, true ), // AND  #
        /* 2A */ (Cpu::op_rol, Cpu::am_imp, "ROL", 2, true ), // ROL  A
        /* 2B */ (Cpu::op_anc, Cpu::am_imm, "ANC", 2, false), // ANC  #
        /* 2C */ (Cpu::op_bit, Cpu::am_abs, "BIT", 4, true ), // BIT  abs
        /* 2D */ (Cpu::op_and, Cpu::am_abs, "AND", 4, true ), // AND  abs
        /* 2E */ (Cpu::op_rol, Cpu::am_abs, "ROL", 6, true ), // ROL  abs
        /* 2F */ (Cpu::op_rla, Cpu::am_abs, "RLA", 6, false), // RLA  abs
        /* 30 */ (Cpu::op_bmi, Cpu::am_rel, "BMI", 2, true ), // BMI  rel
        /* 31 */ (Cpu::op_and, Cpu::am_izy, "AND", 5, true ), // AND  ind,Y
        /* 32 */ (Cpu::op_jam, Cpu::am_imp, "JAM", 2, false), // JAM
        /* 33 */ (Cpu::op_rla, Cpu::am_izy, "RLA", 8, false), // RLA  ind,Y
        /* 34 */ (Cpu::op_nop, Cpu::am_zpx, "NOP", 4, false), // NOP  zpg,X
        /* 35 */ (Cpu::op_and, Cpu::am_zpx, "AND", 4, true ), // AND  zpg,X
        /* 36 */ (Cpu::op_rol, Cpu::am_zpx, "ROL", 6, true ), // ROL  zpg,X
        /* 37 */ (Cpu::op_rla, Cpu::am_zpx, "RLA", 6, false), // RLA  zpg,X
        /* 38 */ (Cpu::op_sec, Cpu::am_imp, "SEC", 2, true ), // SEC  impl
        /* 39 */ (Cpu::op_and, Cpu::am_aby, "AND", 4, true ), // AND  abs,Y
        /* 3A */ (Cpu::op_nop, Cpu::am_imp, "NOP", 2, false), // NOP  impl
        /* 3B */ (Cpu::op_rla, Cpu::am_aby, "RLA", 7, false), // RLA  abs,Y
        /* 3C */ (Cpu::op_nop, Cpu::am_abx, "NOP", 4, false), // NOP  abs,X
        /* 3D */ (Cpu::op_and, Cpu::am_abx, "AND", 4, true ), // AND  abs,X
        /* 3E */ (Cpu::op_rol, Cpu::am_abx, "ROL", 7, true ), // ROL  abs,X
        /* 3F */ (Cpu::op_rla, Cpu::am_abx, "RLA", 7, false), // RLA  abs,X
        /* 40 */ (Cpu::op_rti, Cpu::am_imp, "RTI", 6, true ), // RTI  impl
        /* 41 */ (Cpu::op_eor, Cpu::am_izx, "EOR", 6, true ), // EOR  X,ind
        /* 42 */ (Cpu::op_jam, Cpu::am_imp, "JAM", 2, false), // JAM
        /* 43 */ (Cpu::op_sre, Cpu::am_izx, "SRE", 8, false), // SRE  X,ind
        /* 44 */ (Cpu::op_nop, Cpu::am_zp0, "NOP", 3, false), // NOP  zpg
        /* 45 */ (Cpu::op_eor, Cpu::am_zp0, "EOR", 3, true ), // EOR  zpg
        /* 46 */ (Cpu::op_lsr, Cpu::am_zp0, "LSR", 5, true ), // LSR  zpg
        /* 47 */ (Cpu::op_sre, Cpu::am_zp0, "SRE", 5, false), // SRE  zpg
        /* 48 */ (Cpu::op_pha, Cpu::am_imp, "PHA", 3, true ), // PHA  impl
        /* 49 */ (Cpu::op_eor, Cpu::am_imm, "EOR", 2, true ), // EOR  #
        /* 4A */ (Cpu::op_lsr, Cpu::am_imp, "LSR", 2, true ), // LSR  A
        /* 4B */ (Cpu::op_alr, Cpu::am_imm, "ALR", 2, false), // ALR  #
        /* 4C */ (Cpu::op_jmp, Cpu::am_abs, "JMP", 3, true ), // JMP  abs
        /* 4D */ (Cpu::op_eor, Cpu::am_abs, "EOR", 4, true ), // EOR  abs
        /* 4E */ (Cpu::op_lsr, Cpu::am_abs, "LSR", 6, true ), // LSR  abs
        /* 4F */ (Cpu::op_sre, Cpu::am_abs, "SRE", 6, false), // SRE  abs
        /* 50 */ (Cpu::op_bvc, Cpu::am_rel, "BVC", 2, true ), // BVC  rel
        /* 51 */ (Cpu::op_eor, Cpu::am_izy, "EOR", 5, true ), // EOR  ind,Y
        /* 52 */ (Cpu::op_jam, Cpu::am_imp, "JAM", 2, false), // JAM
        /* 53 */ (Cpu::op_sre, Cpu::am_izy, "SRE", 8, false), // SRE  ind,Y
        /* 54 */ (Cpu::op_nop, Cpu::am_zpx, "NOP", 4, false), // NOP  zpg,X
        /* 55 */ (Cpu::op_eor, Cpu::am_zpx, "EOR", 4, true ), // EOR  zpg,X
        /* 56 */ (Cpu::op_lsr, Cpu::am_zpx, "LSR", 6, true ), // LSR  zpg,X
        /* 57 */ (Cpu::op_sre, Cpu::am_zpx, "SRE", 6, false), // SRE  zpg,X
        /* 58 */ (Cpu::op_cli, Cpu::am_imp, "CLI", 2, true ), // CLI  impl
        /* 59 */ (Cpu::op_eor, Cpu::am_aby, "EOR", 4, true ), // EOR  abs,Y
        /* 5A */ (Cpu::op_nop, Cpu::am_imp, "NOP", 2, false), // NOP  impl
        /* 5B */ (Cpu::op_sre, Cpu::am_aby, "SRE", 7, false), // SRE  abs,Y
        /* 5C */ (Cpu::op_nop, Cpu::am_abx, "NOP", 4, false), // NOP  abs,X
        /* 5D */ (Cpu::op_eor, Cpu::am_abx, "EOR", 4, true ), // EOR  abs,X
        /* 5E */ (Cpu::op_lsr, Cpu::am_abx, "LSR", 7, true ), // LSR  abs,X
        /* 5F */ (Cpu::op_sre, Cpu::am_abx, "SRE", 7, false), // SRE  abs,X
        /* 60 */ (Cpu::op_rts, Cpu::am_imp, "RTS", 6, true ), // RTS  impl
        /* 61 */ (Cpu::op_adc, Cpu::am_izx, "ADC", 6, true ), // ADC  X,ind
        /* 62 */ (Cpu::op_jam, Cpu::am_imp, "JAM", 2, false), // JAM
        /* 63 */ (Cpu::op_rra, Cpu::am_izx, "RRA", 8, false), // RRA  X,ind
        /* 64 */ (Cpu::op_nop, Cpu::am_zp0, "NOP", 3, false), // NOP  zpg
        /* 65 */ (Cpu::op_adc, Cpu::am_zp0, "ADC", 3, true ), // ADC  zpg
        /* 66 */ (Cpu::op_ror, Cpu::am_zp0, "ROR", 5, true ), // ROR  zpg
        /* 67 */ (Cpu::op_rra, Cpu::am_zp0, "RRA", 5, false), // RRA  zpg
        /* 68 */ (Cpu::op_pla, Cpu::am_imp, "PLA", 4, true ), // PLA  impl
        /* 69 */ (Cpu::op_adc, Cpu::am_imm, "ADC", 2, true ), // ADC  #
        /* 6A */ (Cpu::op_ror, Cpu::am_imp, "ROR", 2, true ), // ROR  A
        /* 6B */ (Cpu::op_arr, Cpu::am_imm, "ARR", 2, false), // ARR  #
        /* 6C */ (Cpu::op_jmp, Cpu::am_ind, "JMP", 5, true ), // JMP  ind
        /* 6D */ (Cpu::op_adc, Cpu::am_abs, "ADC", 4, true ), // ADC  abs
        /* 6E */ (Cpu::op_ror, Cpu::am_abs, "ROR", 6, true ), // ROR  abs
        /* 6F */ (Cpu::op_rra, Cpu::am_abs, "RRA", 6, false), // RRA  abs
        /* 70 */ (Cpu::op_bvs, Cpu::am_rel, "BVS", 2, true ), // BVS  rel
        /* 71 */ (Cpu::op_adc, Cpu::am_izy, "ADC", 5, true ), // ADC  ind,Y
        /* 72 */ (Cpu::op_jam, Cpu::am_imp, "JAM", 2, false), // JAM
        /* 73 */ (Cpu::op_rra, Cpu::am_izy, "RRA", 8, false), // RRA  ind,Y
        /* 74 */ (Cpu::op_nop, Cpu::am_zpx, "NOP", 4, false), // NOP  zpg,X
        /* 75 */ (Cpu::op_adc, Cpu::am_zpx, "ADC", 4, true ), // ADC  zpg,X
        /* 76 */ (Cpu::op_ror, Cpu::am_zpx, "ROR", 6, true ), // ROR  zpg,X
        /* 77 */ (Cpu::op_rra, Cpu::am_zpx, "RRA", 6, false), // RRA  zpg,X
        /* 78 */ (Cpu::op_sei, Cpu::am_imp, "SEI", 2, true ), // SEI  impl
        /* 79 */ (Cpu::op_adc, Cpu::am_aby, "ADC", 4, true ), // ADC  abs,Y
        /* 7A */ (Cpu::op_nop, Cpu::am_imp, "NOP", 2, false), // NOP  impl
        /* 7B */ (Cpu::op_rra, Cpu::am_aby, "RRA", 7, false), // RRA  abs,Y
        /* 7C */ (Cpu::op_nop, Cpu::am_abx, "NOP", 4, false), // NOP  abs,X
        /* 7D */ (Cpu::op_adc, Cpu::am_abx, "ADC", 4, true ), // ADC  abs,X
        /* 7E */ (Cpu::op_ror, Cpu::am_abx, "ROR", 7, true ), // ROR  abs,X
        /* 7F */ (Cpu::op_rra, Cpu::am_abx, "RRA", 7, false), // RRA  abs,X
        /* 80 */ (Cpu::op_nop, Cpu::am_imm, "NOP", 2, false), // NOP  #
        /* 81 */ (Cpu::op_sta, Cpu::am_izx, "STA", 6, true ), // STA  X,ind
        /* 82 */ (Cpu::op_nop, Cpu::am_imm, "NOP", 2, false), // NOP  #
        /* 83 */ (Cpu::op_sax, Cpu::am_izx, "SAX", 6, false), // SAX  X,ind
        /* 84 */ (Cpu::op_sty, Cpu::am_zp0, "STY", 3, true ), // STY  zpg
        /* 85 */ (Cpu::op_sta, Cpu::am_zp0, "STA", 3, true ), // STA  zpg
        /* 86 */ (Cpu::op_stx, Cpu::am_zp0, "STX", 3, true ), // STX  zpg
        /* 87 */ (Cpu::op_sax, Cpu::am_zp0, "SAX", 3, false), // SAX  zpg
        /* 88 */ (Cpu::op_dey, Cpu::am_imp, "DEY", 2, true ), // DEY  impl
        /* 89 */ (Cpu::op_nop, Cpu::am_imm, "NOP", 2, false), // NOP  #
        /* 8A */ (Cpu::op_txa, Cpu::am_imp, "TXA", 2, true ), // TXA  impl
        /* 8B */ (Cpu::op_ane, Cpu::am_imm, "ANE", 2, false), // ANE  #
        /* 8C */ (Cpu::op_sty, Cpu::am_abs, "STY", 4, true ), // STY  abs
        /* 8D */ (Cpu::op_sta, Cpu::am_abs, "STA", 4, true ), // STA  abs
        /* 8E */ (Cpu::op_stx, Cpu::am_abs, "STX", 4, true ), // STX  abs
        /* 8F */ (Cpu::op_sax, Cpu::am_abs, "SAX", 4, false), // SAX  abs
        /* 90 */ (Cpu::op_bcc, Cpu::am_rel, "BCC", 2, true ), // BCC  rel
        /* 91 */ (Cpu::op_sta, Cpu::am_izy, "STA", 6, true ), // STA  ind,Y
        /* 92 */ (Cpu::op_jam, Cpu::am_imp, "JAM", 2, false), // JAM
        /* 93 */ (Cpu::op_sha, Cpu::am_izy, "SHA", 6, false), // SHA  ind,Y
        /* 94 */ (Cpu::op_sty, Cpu::am_zpx, "STY", 4, true ), // STY  zpg,X
        /* 95 */ (Cpu::op_sta, Cpu::am_zpx, "STA", 4, true ), // STA  zpg,X
        /* 96 */ (Cpu::op_stx, Cpu::am_zpy, "STX", 4, true ), // STX  zpg,Y
        /* 97 */ (Cpu::op_sax, Cpu::am_zpy, "SAX", 4, false), // SAX  zpg,Y
        /* 98 */ (Cpu::op_tya, Cpu::am_imp, "TYA", 2, true ), // TYA  impl
        /* 99 */ (Cpu::op_sta, Cpu::am_aby, "STA", 5, true ), // STA  abs,Y
        /* 9A */ (Cpu::op_txs, Cpu::am_imp, "TXS", 2, true ), // TXS  impl
        /* 9B */ (Cpu::op_tas, Cpu::am_aby, "TAS", 5, false), // TAS  abs,Y
        /* 9C */ (Cpu::op_shy, Cpu::am_abx, "SHY", 5, false), // SHY  abs,X
        /* 9D */ (Cpu::op_sta, Cpu::am_abx, "STA", 5, true ), // STA  abs,X
        /* 9E */ (Cpu::op_shx, Cpu::am_aby, "SHX", 5, false), // SHX  abs,Y
        /* 9F */ (Cpu::op_sha, Cpu::am_aby, "SHA", 5, false), // SHA  abs,Y
        /* A0 */ (Cpu::op_ldy, Cpu::am_imm, "LDY", 2, true ), // LDY  #
        /* A1 */ (Cpu::op_lda, Cpu::am_izx, "LDA", 6, true ), // LDA  X,ind
        /* A2 */ (Cpu::op_ldx, Cpu::am_imm, "LDX", 2, true ), // LDX  #
        /* A3 */ (Cpu::op_lax, Cpu::am_izx, "LAX", 6, false), // LAX  X,ind
        /* A4 */ (Cpu::op_ldy, Cpu::am_zp0, "LDY", 3, true ), // LDY  zpg
        /* A5 */ (Cpu::op_lda, Cpu::am_zp0, "LDA", 3, true ), // LDA  zpg
        /* A6 */ (Cpu::op_ldx, Cpu::am_zp0, "LDX", 3, true ), // LDX  zpg
        /* A7 */ (Cpu::op_lax, Cpu::am_zp0, "LAX", 3, false), // LAX  zpg
        /* A8 */ (Cpu::op_tay, Cpu::am_imp, "TAY", 2, true ), // TAY  impl
        /* A9 */ (Cpu::op_lda, Cpu::am_imm, "LDA", 2, true ), // LDA  #
        /* AA */ (Cpu::op_tax, Cpu::am_imp, "TAX", 2, true ), // TAX  impl
        /* AB */ (Cpu::op_lxa, Cpu::am_imm, "LXA", 2, false), // LXA  #
        /* AC */ (Cpu::op_ldy, Cpu::am_abs, "LDY", 4, true ), // LDY  abs
        /* AD */ (Cpu::op_lda, Cpu::am_abs, "LDA", 4, true ), // LDA  abs
        /* AE */ (Cpu::op_ldx, Cpu::am_abs, "LDX", 4, true ), // LDX  abs
        /* AF */ (Cpu::op_lax, Cpu::am_abs, "LAX", 4, false), // LAX  abs
        /* B0 */ (Cpu::op_bcs, Cpu::am_rel, "BCS", 2, true ), // BCS  rel
        /* B1 */ (Cpu::op_lda, Cpu::am_izy, "LDA", 5, true ), // LDA  ind,Y
        /* B2 */ (Cpu::op_jam, Cpu::am_imp, "JAM", 2, false), // JAM
        /* B3 */ (Cpu::op_lax, Cpu::am_izy, "LAX", 5, false), // LAX  ind,Y
        /* B4 */ (Cpu::op_ldy, Cpu::am_zpx, "LDY", 4, true ), // LDY  zpg,X
        /* B5 */ (Cpu::op_lda, Cpu::am_zpx, "LDA", 4, true ), // LDA  zpg,X
        /* B6 */ (Cpu::op_ldx, Cpu::am_zpy, "LDX", 4, true ), // LDX  zpg,Y
        /* B7 */ (Cpu::op_lax, Cpu::am_zpy, "LAX", 4, false), // LAX  zpg,Y
        /* B8 */ (Cpu::op_clv, Cpu::am_imp, "CLV", 2, true ), // CLV  impl
        /* B9 */ (Cpu::op_lda, Cpu::am_aby, "LDA", 4, true ), // LDA  abs,Y
        /* BA */ (Cpu::op_tsx, Cpu::am_imp, "TSX", 2, true ), // TSX  impl
        /* BB */ (Cpu::op_las, Cpu::am_aby, "LAS", 4, false), // LAS  abs,Y
        /* BC */ (Cpu::op_ldy, Cpu::am_abx, "LDY", 4, true ), // LDY  abs,X
        /* BD */ (Cpu::op_lda, Cpu::am_abx, "LDA", 4, true ), // LDA  abs,X
        /* BE */ (Cpu::op_ldx, Cpu::am_aby, "LDX", 4, true ), // LDX  abs,Y
        /* BF */ (Cpu::op_lax, Cpu::am_aby, "LAX", 4, false), // LAX  abs,Y
        /* C0 */ (Cpu::op_cpy, Cpu::am_imm, "CPY", 2, true ), // CPY  #
        /* C1 */ (Cpu::op_cmp, Cpu::am_izx, "CMP", 6, true ), // CMP  X,ind
        /* C2 */ (Cpu::op_nop, Cpu::am_imm, "NOP", 2, false), // NOP  #
        /* C3 */ (Cpu::op_dcp, Cpu::am_izx, "DCP", 8, false), // DCP  X,ind
        /* C4 */ (Cpu::op_cpy, Cpu::am_zp0, "CPY", 3, true ), // CPY  zpg
        /* C5 */ (Cpu::op_cmp, Cpu::am_zp0, "CMP", 3, true ), // CMP  zpg
        /* C6 */ (Cpu::op_dec, Cpu::am_zp0, "DEC", 5, true ), // DEC  zpg
        /* C7 */ (Cpu::op_dcp, Cpu::am_zp0, "DCP", 5, false), // DCP  zpg
        /* C8 */ (Cpu::op_iny, Cpu::am_imp, "INY", 2, true ), // INY  impl
        /* C9 */ (Cpu::op_cmp, Cpu::am_imm, "CMP", 2, true ), // CMP  #
        /* CA */ (Cpu::op_dex, Cpu::am_imp, "DEX", 2, true ), // DEX  impl
        /* CB */ (Cpu::op_sbx, Cpu::am_imm, "SBX", 2, false), // SBX  #
        /* CC */ (Cpu::op_cpy, Cpu::am_abs, "CPY", 4, true ), // CPY  abs
        /* CD */ (Cpu::op_cmp, Cpu::am_abs, "CMP", 4, true ), // CMP  abs
        /* CE */ (Cpu::op_dec, Cpu::am_abs, "DEC", 6, true ), // DEC  abs
        /* CF */ (Cpu::op_dcp, Cpu::am_abs, "DCP", 6, false), // DCP  abs
        /* D0 */ (Cpu::op_bne, Cpu::am_rel, "BNE", 2, true ), // BNE  rel
        /* D1 */ (Cpu::op_cmp, Cpu::am_izy, "CMP", 5, true ), // CMP  ind,Y
        /* D2 */ (Cpu::op_jam, Cpu::am_imp, "JAM", 2, false), // JAM
        /* D3 */ (Cpu::op_dcp, Cpu::am_izy, "DCP", 8, false), // DCP  ind,Y
        /* D4 */ (Cpu::op_nop, Cpu::am_zpx, "NOP", 4, false), // NOP  zpg,X
        /* D5 */ (Cpu::op_cmp, Cpu::am_zpx, "CMP", 4, true ), // CMP  zpg,X
        /* D6 */ (Cpu::op_dec, Cpu::am_zpx, "DEC", 6, true ), // DEC  zpg,X
        /* D7 */ (Cpu::op_dcp, Cpu::am_zpx, "DCP", 6, false), // DCP  zpg,X
        /* D8 */ (Cpu::op_cld, Cpu::am_imp, "CLD", 2, true ), // CLD  impl
        /* D9 */ (Cpu::op_cmp, Cpu::am_aby, "CMP", 4, true ), // CMP  abs,Y
        /* DA */ (Cpu::op_nop, Cpu::am_imp, "NOP", 2, false), // NOP  impl
        /* DB */ (Cpu::op_dcp, Cpu::am_aby, "DCP", 7, false), // DCP  abs,Y
        /* DC */ (Cpu::op_nop, Cpu::am_abx, "NOP", 4, false), // NOP  abs,X
        /* DD */ (Cpu::op_cmp, Cpu::am_abx, "CMP", 4, true ), // CMP  abs,X
        /* DE */ (Cpu::op_dec, Cpu::am_abx, "DEC", 7, true ), // DEC  abs,X
        /* DF */ (Cpu::op_dcp, Cpu::am_abx, "DCP", 7, false), // DCP  abs,X
        /* E0 */ (Cpu::op_cpx, Cpu::am_imm, "CPX", 2, true ), // CPX  #
        /* E1 */ (Cpu::op_sbc, Cpu::am_izx, "SBC", 6, true ), // SBC  X,ind
        /* E2 */ (Cpu::op_nop, Cpu::am_imm, "NOP", 2, false), // NOP  #
        /* E3 */ (Cpu::op_isc, Cpu::am_izx, "ISC", 8, false), // ISC  X,ind
        /* E4 */ (Cpu::op_cpx, Cpu::am_zp0, "CPX", 3, true ), // CPX  zpg
        /* E5 */ (Cpu::op_sbc, Cpu::am_zp0, "SBC", 3, true ), // SBC  zpg
        /* E6 */ (Cpu::op_inc, Cpu::am_zp0, "INC", 5, true ), // INC  zpg
        /* E7 */ (Cpu::op_isc, Cpu::am_zp0, "ISC", 5, false), // ISC  zpg
        /* E8 */ (Cpu::op_inx, Cpu::am_imp, "INX", 2, true ), // INX  impl
        /* E9 */ (Cpu::op_sbc, Cpu::am_imm, "SBC", 2, true ), // SBC  #
        /* EA */ (Cpu::op_nop, Cpu::am_imp, "NOP", 2, true ), // NOP  impl
        /* EB */ (Cpu::op_sbc, Cpu::am_imm, "SBC", 2, false), // USBC #
        /* EC */ (Cpu::op_cpx, Cpu::am_abs, "CPX", 4, true ), // CPX  abs
        /* ED */ (Cpu::op_sbc, Cpu::am_abs, "SBC", 4, true ), // SBC  abs
        /* EE */ (Cpu::op_inc, Cpu::am_abs, "INC", 6, true ), // INC  abs
        /* EF */ (Cpu::op_isc, Cpu::am_abs, "ISC", 6, false), // ISC  abs
        /* F0 */ (Cpu::op_beq, Cpu::am_rel, "BEQ", 2, true ), // BEQ  rel
        /* F1 */ (Cpu::op_sbc, Cpu::am_izy, "SBC", 5, true ), // SBC  ind,Y
        /* F2 */ (Cpu::op_jam, Cpu::am_imp, "JAM", 2, false), // JAM
        /* F3 */ (Cpu::op_isc, Cpu::am_izy, "ISC", 8, false), // ISC  ind,Y
        /* F4 */ (Cpu::op_nop, Cpu::am_zpx, "NOP", 4, false), // NOP  zpg,X
        /* F5 */ (Cpu::op_sbc, Cpu::am_zpx, "SBC", 4, true ), // SBC  zpg,X
        /* F6 */ (Cpu::op_inc, Cpu::am_zpx, "INC", 6, true ), // INC  zpg,X
        /* F7 */ (Cpu::op_isc, Cpu::am_zpx, "ISC", 6, false), // ISC  zpg,X
        /* F8 */ (Cpu::op_sed, Cpu::am_imp, "SED", 2, true ), // SED  impl
        /* F9 */ (Cpu::op_sbc, Cpu::am_aby, "SBC", 4, true ), // SBC  abs,Y
        /* FA */ (Cpu::op_nop, Cpu::am_imp, "NOP", 2, false), // NOP  impl
        /* FB */ (Cpu::op_isc, Cpu::am_aby, "ISC", 7, false), // ISC  abs,Y
        /* FC */ (Cpu::op_nop, Cpu::am_abx, "NOP", 4, false), // NOP  abs,X
        /* FD */ (Cpu::op_sbc, Cpu::am_abx, "SBC", 4, true ), // SBC  abs,X
        /* FE */ (Cpu::op_inc, Cpu::am_abx, "INC", 7, true ), // INC  abs,X
        /* FF */ (Cpu::op_isc, Cpu::am_abx, "ISC", 7, false), // ISC  abs,X
    ];

    /// Fetch a single byte from the bus at the [program_counter] position and increment the
    /// [program_counter] by 1.
    fn fetch(&mut self) -> u8 {
        let value = self.bus.read(self.state.program_counter);
        self.state.program_counter = self.state.program_counter.wrapping_add(1);
        value
    }

    fn am_imp(&mut self) -> Operand {
        Operand::None
    }

    fn am_imm(&mut self) -> Operand {
        Operand::Immediate(self.fetch())
    }

    fn am_zp0(&mut self) -> Operand {
        Operand::ZeroPage(self.fetch(), Offset::None)
    }

    fn am_zpx(&mut self) -> Operand {
        Operand::ZeroPage(self.fetch(), Offset::X)
    }

    fn am_zpy(&mut self) -> Operand {
        Operand::ZeroPage(self.fetch(), Offset::Y)
    }

    fn am_rel(&mut self) -> Operand {
        Operand::Relative(self.fetch() as i8)
    }

    fn am_abs(&mut self) -> Operand {
        Operand::Absolute(
            u16::from_le_bytes([self.fetch(), self.fetch()]),
            Offset::None,
        )
    }

    fn am_abx(&mut self) -> Operand {
        Operand::Absolute(u16::from_le_bytes([self.fetch(), self.fetch()]), Offset::X)
    }

    fn am_aby(&mut self) -> Operand {
        Operand::Absolute(u16::from_le_bytes([self.fetch(), self.fetch()]), Offset::Y)
    }

    fn am_ind(&mut self) -> Operand {
        Operand::Indirect(u16::from_le_bytes([self.fetch(), self.fetch()]))
    }

    fn am_izx(&mut self) -> Operand {
        Operand::IndirectZeroPage(self.fetch(), Offset::X)
    }

    fn am_izy(&mut self) -> Operand {
        Operand::IndirectZeroPage(self.fetch(), Offset::Y)
    }

    // Add Memory to Accumulator with Carry
    //
    // A + M + C -> A, C
    // N Z C I D V
    // + + + - - +
    // addressing       assembler       opc     bytes   cycles
    // immediate        ADC #oper       69      2       2
    // zeropage         ADC oper        65      2       3
    // zeropage,X       ADC oper,X      75      2       4
    // absolute         ADC oper        6D      3       4
    // absolute,X       ADC oper,X      7D      3       4*
    // absolute,Y       ADC oper,Y      79      3       4*
    // (indirect,X)     ADC (oper,X)    61      2       6
    // (indirect),Y     ADC (oper),Y    71      2       5*
    fn op_adc(&mut self, __operand: &Operand) {
        let (result, carry) = {
            let carry = match self.state.flags.contains(Flags::CARRY) {
                true => 1,
                false => 0,
            };

            let result = (self.state.accumulator as u16)
                .wrapping_add(self.internal.immediate as u16)
                .wrapping_add(carry);

            (result as u8, result & 0xFF00 != 0)
        };

        self.flags_from_result(result);

        self.state.flags.set(Flags::OVERFLOW, {
            let p1 = ((result.bitxor(self.state.accumulator)) & 0x80) != 0;
            let p2 = ((self.internal.immediate.bitxor(self.state.accumulator)) & 0x80) == 0;

            p1 && p2
        });

        self.state.flags.set(Flags::CARRY, carry);

        self.state.accumulator = result;
    }

    fn op_alr(&mut self, _operand: &Operand) {
        todo!()
    }

    fn op_anc(&mut self, _operand: &Operand) {
        todo!()
    }

    // AND Memory with Accumulator
    //
    // A AND M -> A
    // N Z C I D V
    // + + - - - -
    // addressing    assembler     opc  bytes  cycles
    // immediate     AND #oper     29   2      2
    // zeropage      AND oper      25   2      3
    // zeropage,X    AND oper,X    35   2      4
    // absolute      AND oper      2D   3      4
    // absolute,X    AND oper,X    3D   3      4*
    // absolute,Y    AND oper,Y    39   3      4*
    // (indirect,X)  AND (oper,X)  21   2      6
    // (indirect),Y  AND (oper),Y  31   2      5*
    fn op_and(&mut self, operand: &Operand) {
        let (value, cycles_required) = self.get_operand_value(operand);
        self.cycles_remaining += cycles_required;

        let result = self.state.accumulator.bitand(value);
        self.flags_from_result(result);
        self.state.accumulator = result;
    }

    fn op_ane(&mut self, _operand: &Operand) {
        todo!()
    }

    fn op_arr(&mut self, _operand: &Operand) {
        todo!()
    }

    // Shift Left One Bit (Memory or Accumulator)
    //
    // C <- [76543210] <- 0
    // N Z C I D V
    // + + + - - -
    // addressing   assembler   opc  bytes  cycles
    // accumulator  ASL A       0A   1      2
    // zeropage     ASL oper    06   2      5
    // zeropage,X   ASL oper,X  16   2      6
    // absolute     ASL oper    0E   3      6
    // absolute,X   ASL oper,X  1E   3      7
    fn op_asl(&mut self, operand: &Operand) {
        let (value, cycles_required) = self.get_operand_value(operand);
        self.cycles_remaining += cycles_required;

        let result = value.wrapping_shl(1);

        self.flags_from_result(result as u8);
        self.state.flags.set(Flags::CARRY, value & 0x80 != 0);

        self.set_operand_value(operand, result as u8);
    }

    // Branch on Carry Clear
    //
    // branch on C = 0
    // N Z C I D V
    // - - - - - -
    // addressing  assembler  opc  bytes  cycles
    // relative    BCC oper   90   2      2**
    fn op_bcc(&mut self, operand: &Operand) {
        self.branch_if_flag(operand, |f| !f.contains(Flags::CARRY));
    }

    // Branch on Carry Set
    //
    // branch on C = 1
    // N Z C I D V
    // - - - - - -
    // addressing  assembler  opc  bytes  cycles
    // relative    BCS oper   B0   2      2**
    fn op_bcs(&mut self, operand: &Operand) {
        self.branch_if_flag(operand, |f| f.contains(Flags::CARRY));
    }

    // Branch on Result Zero
    //
    // branch on Z = 1
    // N Z C I D V
    // - - - - - -
    // addressing  assembler  opc  bytes  cycles
    // relative    BEQ oper   F0   2      2**
    fn op_beq(&mut self, operand: &Operand) {
        self.branch_if_flag(operand, |f| f.contains(Flags::ZERO));
    }

    // Test Bits in Memory with Accumulator
    //
    // bits 7 and 6 of operand are transferred to bit 7 and 6 of SR (N,V);
    // the zero-flag is set to the result of operand AND accumulator.
    //
    // A AND M, M7 -> N, M6 -> V
    // N  Z C I D V
    // M7 + - - - M6
    // addressing  assembler  opc  bytes  cycles
    // zeropage    BIT oper   24   2      3
    // absolute    BIT oper   2C   3      4
    fn op_bit(&mut self, operand: &Operand) {
        let (value, cycles_required) = self.get_operand_value(operand);
        self.cycles_remaining += cycles_required;

        self.state
            .flags
            .set(Flags::ZERO, self.state.accumulator.bitand(value) == 0);

        self.state
            .flags
            .set(Flags::NEGATIVE, value.bitand(Flags::NEGATIVE.bits()) != 0);
        self.state
            .flags
            .set(Flags::OVERFLOW, value.bitand(Flags::OVERFLOW.bits()) != 0);
    }

    // Branch on Result Minus
    //
    // branch on N = 1
    // N Z C I D V
    // - - - - - -
    // addressing  assembler  opc  bytes  cycles
    // relative    BMI oper   30   2      2**
    fn op_bmi(&mut self, operand: &Operand) {
        self.branch_if_flag(operand, |f| f.contains(Flags::NEGATIVE));
    }

    // Branch on Result not Zero
    //
    // branch on Z = 0
    // N Z C I D V
    // - - - - - -
    // addressing  assembler  opc  bytes  cycles
    // relative    BNE oper   D0   2      2**
    fn op_bne(&mut self, operand: &Operand) {
        self.branch_if_flag(operand, |f| !f.contains(Flags::ZERO));
    }

    // Branch on Result Plus
    //
    // branch on N = 0
    // N Z C I D V
    // - - - - - -
    // addressing  assembler  opc  bytes  cycles
    // relative    BPL oper   10   2      2**
    fn op_bpl(&mut self, operand: &Operand) {
        self.branch_if_flag(operand, |f| !f.contains(Flags::NEGATIVE));
    }

    fn op_brk(&mut self, _operand: &Operand) {
        todo!()
    }

    // Branch on Overflow Clear
    //
    // branch on V = 0
    // N Z C I D V
    // - - - - - -
    // addressing  assembler  opc  bytes  cycles
    // relative    BVC oper   50   2      2**
    fn op_bvc(&mut self, operand: &Operand) {
        self.branch_if_flag(operand, |f| !f.contains(Flags::OVERFLOW));
    }

    // Branch on Overflow Set
    //
    // branch on V = 1
    // N Z C I D V
    // - - - - - -
    // addressing  assembler  opc  bytes  cycles
    // relative    BVS oper   70   2      2**
    fn op_bvs(&mut self, operand: &Operand) {
        self.branch_if_flag(operand, |f| f.contains(Flags::OVERFLOW));
    }

    // Clear Carry Flag
    //
    // 0 -> C
    // N Z C I D V
    // - - 0 - - -
    // addressing  assembler  opc  bytes  cycles
    // implied     CLC        18   1      2
    fn op_clc(&mut self, _operand: &Operand) {
        self.state.flags.set(Flags::CARRY, false);
    }

    // Clear Decimal Mode
    //
    // 0 -> D
    // N Z C I D V
    // - - - - 0 -
    // addressing  assembler  opc  bytes  cycles
    // implied     CLD        D8   1      2
    fn op_cld(&mut self, _operand: &Operand) {
        self.state.flags.set(Flags::DECIMAL, false);
    }

    fn op_cli(&mut self, _operand: &Operand) {
        todo!()
    }

    // Clear Overflow Flag
    //
    // 0 -> V
    // N Z C I D V
    // - - - - - 0
    // addressing  assembler  opc  bytes  cycles
    // implied     CLV        B8   1      2
    fn op_clv(&mut self, _operand: &Operand) {
        self.state.flags.set(Flags::OVERFLOW, false);
    }

    // Compare Memory with Accumulator
    //
    // A - M
    // N Z C I D V
    // + + + - - -
    // addressing    assembler     opc  bytes  cycles
    // immediate     CMP #oper     C9   2      2
    // zeropage      CMP oper      C5   2      3
    // zeropage,X    CMP oper,X    D5   2      4
    // absolute      CMP oper      CD   3      4
    // absolute,X    CMP oper,X    DD   3      4*
    // absolute,Y    CMP oper,Y    D9   3      4*
    // (indirect,X)  CMP (oper,X)  C1   2      6
    // (indirect),Y  CMP (oper),Y  D1   2      5*
    fn op_cmp(&mut self, operand: &Operand) {
        let (value, cycles_required) = self.get_operand_value(operand);
        self.cycles_remaining += cycles_required;

        self.operation_compare(self.state.accumulator, value);
    }

    // Compare Memory and Index X
    //
    // X - M
    // N Z C I D V
    // + + + - - -
    // addressing  assembler  opc  bytes  cycles
    // immediate   CPX #oper  E0   2      2
    // zeropage    CPX oper   E4   2      3
    // absolute    CPX oper   EC   3      4
    fn op_cpx(&mut self, operand: &Operand) {
        let (value, cycles_required) = self.get_operand_value(operand);
        self.cycles_remaining += cycles_required;

        self.operation_compare(self.state.x_register, value);
    }

    // Compare Memory and Index Y
    //
    // Y - M
    // N Z C I D V
    // + + + - - -
    // addressing  assembler  opc  bytes  cycles
    // immediate   CPY #oper  C0   2      2
    // zeropage    CPY oper   C4   2      3
    // absolute    CPY oper   CC   3      4
    fn op_cpy(&mut self, operand: &Operand) {
        let (value, cycles_required) = self.get_operand_value(operand);
        self.cycles_remaining += cycles_required;

        self.operation_compare(self.state.y_register, value);
    }

    // DEC oper + CMP oper
    //
    // M - 1 -> M, A - M
    //
    // N Z C I D V
    // + + + - - -
    // addressing    assembler     opc  bytes  cycles
    // zeropage      DCP oper      C7   2      5
    // zeropage,X    DCP oper,X    D7   2      6
    // absolute      DCP oper      CF   3      6
    // absolut,X     DCP oper,X    DF   3      7
    // absolut,Y     DCP oper,Y    DB   3      7
    // (indirect,X)  DCP (oper,X)  C3   2      8
    // (indirect),Y  DCP (oper),Y  D3   2      8
    fn op_dcp(&mut self, operand: &Operand) {
        let (value, _cycles_required) = self.get_operand_value(operand);
        // TODO: According to nestest, we should not include extra cycles here.
        // self.cycles_remaining += cycles_required;

        // DEC
        let result = value.wrapping_sub(1);
        self.flags_from_result(result);

        self.set_operand_value(operand, result);

        // CMP
        self.operation_compare(self.state.accumulator, result);
    }

    // Decrement Memory by One
    //
    // M - 1 -> M
    // N Z C I D V
    // + + - - - -
    // addressing  assembler   opc  bytes  cycles
    // zeropage    DEC oper    C6   2      5
    // zeropage,X  DEC oper,X  D6   2      6
    // absolute    DEC oper    CE   3      6
    // absolute,X  DEC oper,X  DE   3      7
    fn op_dec(&mut self, operand: &Operand) {
        let (value, cycles_required) = self.get_operand_value(operand);
        self.cycles_remaining += cycles_required;

        let result = value.wrapping_sub(1);
        self.flags_from_result(result);

        self.set_operand_value(operand, result);
    }

    // Decrement Index X by One
    //
    // X - 1 -> X
    // N Z C I D V
    // + + - - - -
    // addressing  assembler  opc  bytes  cycles
    // implied     DEX        CA   1      2
    fn op_dex(&mut self, _operand: &Operand) {
        let result = self.state.x_register.wrapping_sub(1);
        self.flags_from_result(result);
        self.state.x_register = result;
    }

    // Decrement Index Y by One
    //
    // Y - 1 -> Y
    // N Z C I D V
    // + + - - - -
    // addressing  assembler  opc  bytes  cycles
    // implied     DEY        88   1      2
    fn op_dey(&mut self, _operand: &Operand) {
        let result = self.state.y_register.wrapping_sub(1);
        self.flags_from_result(result);
        self.state.y_register = result;
    }

    // Exclusive-OR Memory with Accumulator
    //
    // A EOR M -> A
    // N Z C I D V
    // + + - - - -
    // addressing    assembler     opc  bytes  cycles
    // immediate     EOR #oper     49   2      2
    // zeropage      EOR oper      45   2      3
    // zeropage,X    EOR oper,X    55   2      4
    // absolute      EOR oper      4D   3      4
    // absolute,X    EOR oper,X    5D   3      4*
    // absolute,Y    EOR oper,Y    59   3      4*
    // (indirect,X)  EOR (oper,X)  41   2      6
    // (indirect),Y  EOR (oper),Y  51   2      5*
    fn op_eor(&mut self, operand: &Operand) {
        let (value, cycles_required) = self.get_operand_value(operand);
        self.cycles_remaining += cycles_required;

        let result = self.state.accumulator.bitxor(value);
        self.flags_from_result(result);
        self.state.accumulator = result;
    }

    // Increment Memory by One
    //
    // M + 1 -> M
    // N Z C I D V
    // + + - - - -
    // addressing  assembler   opc  bytes  cycles
    // zeropage    INC oper    E6   2      5
    // zeropage,X  INC oper,X  F6   2      6
    // absolute    INC oper    EE   3      6
    // absolute,X  INC oper,X  FE   3      7
    fn op_inc(&mut self, operand: &Operand) {
        let (value, cycles_required) = self.get_operand_value(operand);
        self.cycles_remaining += cycles_required;

        let result = value.wrapping_add(1);

        self.flags_from_result(result);
        self.set_operand_value(operand, result);
    }

    // Increment Index X by One
    //
    // X + 1 -> X
    // N Z C I D V
    // + + - - - -
    // addressing  assembler  opc  bytes  cycles
    // implied     INX        E8   1      2
    fn op_inx(&mut self, _operand: &Operand) {
        let result = self.state.x_register.wrapping_add(1);
        self.flags_from_result(result);
        self.state.x_register = result;
    }

    // Increment Index Y by One
    //
    // Y + 1 -> Y
    // N Z C I D V
    // + + - - - -
    // addressing  assembler  opc  bytes  cycles
    // implied     INY        C8   1      2
    fn op_iny(&mut self, _operand: &Operand) {
        let result = self.state.y_register.wrapping_add(1);
        self.flags_from_result(result);
        self.state.y_register = result;
    }

    // INC oper + SBC oper
    //
    // M + 1 -> M, A - M - C -> A
    //
    // N Z C I D V
    // + + + - - +
    // addressing    assembler     opc  bytes  cycles
    // zeropage      ISC oper      E7   2      5
    // zeropage,X    ISC oper,X    F7   2      6
    // absolute      ISC oper      EF   3      6
    // absolut,X     ISC oper,X    FF   3      7
    // absolut,Y     ISC oper,Y    FB   3      7
    // (indirect,X)  ISC (oper,X)  E3   2      8
    // (indirect),Y  ISC (oper),Y  F3   2      4
    fn op_isc(&mut self, operand: &Operand) {
        let (value, _cycles_required) = self.get_operand_value(operand);
        // self.cycles_remaining += cycles_required;

        // INC
        let result = value.wrapping_add(1);

        self.flags_from_result(result);
        self.set_operand_value(operand, result);

        // SBC
        let (result, overflow) = {
            let carry = match self.state.flags.contains(Flags::CARRY) {
                true => 0,
                false => 1,
            };

            let result = (self.state.accumulator as u16)
                .wrapping_sub(result as u16)
                .wrapping_sub(carry);

            (result as u8, result & 0xFF00 != 0)
        };

        self.state.flags.set(Flags::OVERFLOW, {
            let p1 = ((result.bitxor(self.state.accumulator)) & 0x80) != 0;
            let p2 = ((value.bitxor(self.state.accumulator)) & 0x80) != 0;

            p1 && p2
        });

        self.state.flags.set(Flags::CARRY, !overflow);

        self.flags_from_result(result);

        self.state.accumulator = result;
    }

    fn op_jam(&mut self, _operand: &Operand) {
        todo!()
    }

    // Jump to New Location
    //
    // (PC+1) -> PCL
    // (PC+2) -> PCH
    // N Z C I D V
    // - - - - - -
    // addressing  assembler   opc  bytes  cycles
    // absolute    JMP oper    4C   3      3
    // indirect    JMP (oper)  6C   3      5
    fn op_jmp(&mut self, operand: &Operand) {
        let (addr, cycles_required) = self.get_absolute_address_for_operand(operand);
        self.cycles_remaining += cycles_required;

        self.state.program_counter = addr;
    }

    // Jump to New Location Saving Return Address
    //
    // push (PC+2),
    // (PC+1) -> PCL
    // (PC+2) -> PCH
    // N Z C I D V
    // - - - - - -
    // addressing  assembler  opc  bytes  cycles
    // absolute    JSR oper   20   3      6
    fn op_jsr(&mut self, operand: &Operand) {
        self.push_16(self.state.program_counter.wrapping_sub(1));

        let (addr, cycles_required) = self.get_absolute_address_for_operand(operand);
        self.cycles_remaining += cycles_required;

        self.state.program_counter = addr;
    }

    fn op_las(&mut self, _operand: &Operand) {
        todo!()
    }

    // LDA oper + LDX oper
    //
    // M -> A -> X
    // N Z C I D V
    // + + - - - -
    // addressing    assembler     opc  bytes  cycles
    // zeropage      LAX oper      A7   2      3
    // zeropage,Y    LAX oper,Y    B7   2      4
    // absolute      LAX oper      AF   3      4
    // absolut,Y     LAX oper,Y    BF   3      4*
    // (indirect,X)  LAX (oper,X)  A3   2      6
    // (indirect),Y  LAX (oper),Y  B3   2      5*
    fn op_lax(&mut self, operand: &Operand) {
        let (value, cycles_required) = self.get_operand_value(operand);
        self.cycles_remaining += cycles_required;

        // LDA
        self.flags_from_result(value);
        self.state.accumulator = value;

        // LDX
        self.flags_from_result(value);
        self.state.x_register = value;
    }

    // Load Accumulator with Memory
    //
    // M -> A
    // N Z C I D V
    // + + - - - -
    // addressing    assembler     opc  bytes  cycles
    // immediate     LDA #oper     A9   2      2
    // zeropage      LDA oper      A5   2      3
    // zeropage,X    LDA oper,X    B5   2      4
    // absolute      LDA oper      AD   3      4
    // absolute,X    LDA oper,X    BD   3      4*
    // absolute,Y    LDA oper,Y    B9   3      4*
    // (indirect,X)  LDA (oper,X)  A1   2      6
    // (indirect),Y  LDA (oper),Y  B1   2      5*
    fn op_lda(&mut self, __operand: &Operand) {
        let value = self.internal.immediate;

        self.flags_from_result(value);
        self.state.accumulator = value;
    }

    // Load Index X with Memory
    //
    // M -> X
    // N Z C I D V
    // + + - - - -
    // addressing  assembler   opc  bytes  cycles
    // immediate   LDX #oper   A2   2      2
    // zeropage    LDX oper    A6   2      3
    // zeropage,Y  LDX oper,Y  B6   2      4
    // absolute    LDX oper    AE   3      4
    // absolute,Y  LDX oper,Y  BE   3      4*
    fn op_ldx(&mut self, __operand: &Operand) {
        let value = self.internal.immediate;

        self.flags_from_result(value);
        self.state.x_register = value;
    }

    // Load Index Y with Memory
    //
    // M -> Y
    // N Z C I D V
    // + + - - - -
    // addressing  assembler   opc  bytes  cycles
    // immediate   LDY #oper   A0   2      2
    // zeropage    LDY oper    A4   2      3
    // zeropage,X  LDY oper,X  B4   2      4
    // absolute    LDY oper    AC   3      4
    // absolute,X  LDY oper,X  BC   3      4*
    fn op_ldy(&mut self, __operand: &Operand) {
        let value = self.internal.immediate;

        self.flags_from_result(value);
        self.state.y_register = value;
    }

    // Shift One Bit Right (Memory or Accumulator)
    //
    // 0 -> [76543210] -> C
    // N Z C I D V
    // 0 + + - - -
    // addressing   assembler   opc  bytes  cycles
    // accumulator  LSR A       4A   1      2
    // zeropage     LSR oper    46   2      5
    // zeropage,X   LSR oper,X  56   2      6
    // absolute     LSR oper    4E   3      6
    // absolute,X   LSR oper,X  5E   3      7
    fn op_lsr(&mut self, operand: &Operand) {
        let (value, cycles_required) = self.get_operand_value(operand);
        self.cycles_remaining += cycles_required;

        self.state.flags.set(Flags::CARRY, value & 0x01 != 0);
        let result = value.wrapping_shr(1);
        self.flags_from_result(result);

        self.set_operand_value(operand, result);
    }

    fn op_lxa(&mut self, _operand: &Operand) {
        todo!()
    }

    // No Operation
    //
    // ---
    // N Z C I D V
    // - - - - - -
    // addressing   assembler   opc     bytes   cycles
    // implied      NOP         EA      1       2
    fn op_nop(&mut self, _operand: &Operand) {}

    // OR Memory with Accumulator
    //
    // A OR M -> A
    // N Z C I D V
    // + + - - - -
    // addressing    assembler     opc  bytes  cycles
    // immediate     ORA #oper     09   2      2
    // zeropage      ORA oper      05   2      3
    // zeropage,X    ORA oper,X    15   2      4
    // absolute      ORA oper      0D   3      4
    // absolute,X    ORA oper,X    1D   3      4*
    // absolute,Y    ORA oper,Y    19   3      4*
    // (indirect,X)  ORA (oper,X)  01   2      6
    // (indirect),Y  ORA (oper),Y  11   2      5*
    fn op_ora(&mut self, operand: &Operand) {
        let (value, cycles_required) = self.get_operand_value(operand);
        self.cycles_remaining += cycles_required;

        let result = self.state.accumulator.bitor(value);
        self.flags_from_result(result);
        self.state.accumulator = result;
    }

    // Push Accumulator on Stack
    //
    // push A
    // N Z C I D V
    // - - - - - -
    // addressing  assembler  opc  bytes  cycles
    // implied     PHA        48   1      3
    fn op_pha(&mut self, _operand: &Operand) {
        self.push(self.state.accumulator);
    }

    // Push Processor Status on Stack
    //
    // The status register will be pushed with the break flag and bit 5 set to 1.
    //
    // push SR
    // N Z C I D V
    // - - - - - -
    // addressing  assembler  opc  bytes  cycles
    // implied     PHP        08   1      3
    fn op_php(&mut self, _operand: &Operand) {
        let flags = self.state.flags | Flags::BREAK | Flags::UNUSED;
        self.push(flags.bits());
    }

    // Pull Accumulator from Stack
    //
    // pull A
    // N Z C I D V
    // + + - - - -
    // addressing  assembler  opc  bytes  cycles
    // implied     PLA        68   1      4
    fn op_pla(&mut self, _operand: &Operand) {
        let result = self.pull();
        self.flags_from_result(result);
        self.state.accumulator = result;
    }

    // Pull Processor Status from Stack
    //
    // The status register will be pulled with the break flag and bit 5 ignored.
    //
    // pull SR
    // N Z C I D V
    // from stack
    // addressing  assembler  opc  bytes  cycles
    // implied     PLP        28   1      4
    fn op_plp(&mut self, _operand: &Operand) {
        let value = self.pull();
        let has_break = self.state.flags.contains(Flags::BREAK);
        let has_unused = self.state.flags.contains(Flags::UNUSED);
        self.state.flags = Flags::from_bits_truncate(value);
        self.state.flags.set(Flags::BREAK, has_break);
        self.state.flags.set(Flags::UNUSED, has_unused);
    }

    // ROL oper + AND oper
    //
    // M = C <- [76543210] <- C, A AND M -> A
    // N Z C I D V
    // + + + - - -
    // addressing    assembler     opc  bytes  cycles
    // zeropage      RLA oper      27   2      5
    // zeropage,X    RLA oper,X    37   2      6
    // absolute      RLA oper      2F   3      6
    // absolut,X     RLA oper,X    3F   3      7
    // absolut,Y     RLA oper,Y    3B   3      7
    // (indirect,X)  RLA (oper,X)  23   2      8
    // (indirect),Y  RLA (oper),Y  33   2      8
    fn op_rla(&mut self, operand: &Operand) {
        let (value, _cycles_required) = self.get_operand_value(operand);
        // self.cycles_remaining += cycles_required;

        // ROL
        let result =
            value
                .wrapping_shl(1)
                .wrapping_add(if self.state.flags.contains(Flags::CARRY) {
                    0x01
                } else {
                    0x00
                });

        self.flags_from_result(result);
        self.state.flags.set(Flags::CARRY, value & 0x80 != 0);

        self.set_operand_value(operand, result);

        // AND
        let result = self.state.accumulator.bitand(result);
        self.flags_from_result(result);
        self.state.accumulator = result;
    }

    // Rotate One Bit Left (Memory or Accumulator)
    //
    // C <- [76543210] <- C
    // N Z C I D V
    // + + + - - -
    // addressing   assembler   opc  bytes  cycles
    // accumulator  ROL A       2A   1      2
    // zeropage     ROL oper    26   2      5
    // zeropage,X   ROL oper,X  36   2      6
    // absolute     ROL oper    2E   3      6
    // absolute,X   ROL oper,X  3E   3      7
    fn op_rol(&mut self, operand: &Operand) {
        let (value, cycles_required) = self.get_operand_value(operand);
        self.cycles_remaining += cycles_required;

        let result =
            value
                .wrapping_shl(1)
                .wrapping_add(if self.state.flags.contains(Flags::CARRY) {
                    0x01
                } else {
                    0x00
                });

        self.flags_from_result(result);
        self.state.flags.set(Flags::CARRY, value & 0x80 != 0);

        self.set_operand_value(operand, result);
    }

    // Rotate One Bit Right (Memory or Accumulator)
    //
    // C -> [76543210] -> C
    // N Z C I D V
    // + + + - - -
    // addressing   assembler   opc  bytes  cycles
    // accumulator  ROR A       6A   1      2
    // zeropage     ROR oper    66   2      5
    // zeropage,X   ROR oper,X  76   2      6
    // absolute     ROR oper    6E   3      6
    // absolute,X   ROR oper,X  7E   3      7
    fn op_ror(&mut self, operand: &Operand) {
        let (value, cycles_required) = self.get_operand_value(operand);
        self.cycles_remaining += cycles_required;

        let result =
            value
                .wrapping_shr(1)
                .wrapping_add(if self.state.flags.contains(Flags::CARRY) {
                    0x80
                } else {
                    0x00
                });

        self.flags_from_result(result);
        self.state.flags.set(Flags::CARRY, value & 0x01 != 0);

        self.set_operand_value(operand, result);
    }

    // ROR oper + ADC oper
    //
    // M = C -> [76543210] -> C, A + M + C -> A, C
    // N Z C I D V
    // + + + - - +
    // addressing    assembler     opc  bytes  cycles
    // zeropage      RRA oper      67   2      5
    // zeropage,X    RRA oper,X    77   2      6
    // absolute      RRA oper      6F   3      6
    // absolut,X     RRA oper,X    7F   3      7
    // absolut,Y     RRA oper,Y    7B   3      7
    // (indirect,X)  RRA (oper,X)  63   2      8
    // (indirect),Y  RRA (oper),Y  73   2      8
    fn op_rra(&mut self, operand: &Operand) {
        let (value, _cycles_required) = self.get_operand_value(operand);
        // self.cycles_remaining += cycles_required;

        // ROR
        let ror_result =
            value
                .wrapping_shr(1)
                .wrapping_add(if self.state.flags.contains(Flags::CARRY) {
                    0x80
                } else {
                    0x00
                });

        self.flags_from_result(ror_result);
        self.state.flags.set(Flags::CARRY, value & 0x01 != 0);

        self.set_operand_value(operand, ror_result);

        // ADC
        let (adc_result, carry) = {
            let carry = match self.state.flags.contains(Flags::CARRY) {
                true => 1,
                false => 0,
            };

            let temp = (self.state.accumulator as u16)
                .wrapping_add(ror_result as u16)
                .wrapping_add(carry);

            (temp as u8, temp & 0xFF00 != 0)
        };

        self.flags_from_result(adc_result);

        self.state.flags.set(Flags::OVERFLOW, {
            let p1 = ((adc_result.bitxor(self.state.accumulator)) & 0x80) != 0;
            let p2 = ((ror_result.bitxor(self.state.accumulator)) & 0x80) == 0;

            p1 && p2
        });

        self.state.flags.set(Flags::CARRY, carry);

        self.state.accumulator = adc_result;
    }

    // Return from Interrupt
    //
    // The status register is pulled with the break flag and bit 5 ignored. Then PC is
    // pulled from the stack.
    //
    // pull SR, pull PC
    // N Z C I D V
    // from stack
    // addressing  assembler  opc  bytes  cycles
    // implied     RTI        40   1      6
    fn op_rti(&mut self, _operand: &Operand) {
        self.state.flags = Flags::from_bits_truncate(self.pull());
        self.state.flags.insert(Flags::UNUSED);
        self.state.flags.remove(Flags::BREAK);

        self.state.program_counter = self.pull_16();
    }

    // Return from Subroutine
    //
    // pull PC, PC+1 -> PC
    // N Z C I D V
    // - - - - - -
    // addressing  assembler  opc  bytes  cycles
    // implied     RTS        60   1      6
    fn op_rts(&mut self, _operand: &Operand) {
        self.state.program_counter = self.pull_16().wrapping_add(1);
    }

    // (AXS, AAX)
    //
    // A and X are put on the bus at the same time (resulting effectively in an AND operation) and stored in M
    //
    // A AND X -> M
    // N Z C I D V
    // - - - - - -
    // addressing    assembler     opc  bytes  cycles
    // zeropage      SAX oper      87   2      3
    // zeropage,Y    SAX oper,Y    97   2      4
    // absolute      SAX oper      8F   3      4
    // (indirect,X)  SAX (oper,X)  83   2      6
    fn op_sax(&mut self, operand: &Operand) {
        let result = self.state.accumulator & self.state.x_register;

        self.set_operand_value(operand, result);
    }

    // Subtract Memory from Accumulator with Borrow
    //
    // A - M - C -> A
    // N Z C I D V
    // + + + - - +
    // addressing    assembler     opc  bytes  cycles
    // immediate     SBC #oper     E9   2      2
    // zeropage      SBC oper      E5   2      3
    // zeropage,X    SBC oper,X    F5   2      4
    // absolute      SBC oper      ED   3      4
    // absolute,X    SBC oper,X    FD   3      4*
    // absolute,Y    SBC oper,Y    F9   3      4*
    // (indirect,X)  SBC (oper,X)  E1   2      6
    // (indirect),Y  SBC (oper),Y  F1   2      5*
    fn op_sbc(&mut self, operand: &Operand) {
        let (value, cycles_required) = self.get_operand_value(operand);
        self.cycles_remaining += cycles_required;

        let (result, overflow) = {
            let carry = match self.state.flags.contains(Flags::CARRY) {
                true => 0,
                false => 1,
            };

            let result = (self.state.accumulator as u16)
                .wrapping_sub(value as u16)
                .wrapping_sub(carry);

            (result as u8, result & 0xFF00 != 0)
        };

        self.state.flags.set(Flags::OVERFLOW, {
            let p1 = ((result.bitxor(self.state.accumulator)) & 0x80) != 0;
            let p2 = ((value.bitxor(self.state.accumulator)) & 0x80) != 0;

            p1 && p2
        });

        self.state.flags.set(Flags::CARRY, !overflow);

        self.flags_from_result(result);

        self.state.accumulator = result;
    }

    fn op_sbx(&mut self, _operand: &Operand) {
        todo!()
    }

    // Set Carry Flag
    //
    // 1 -> C
    // N Z C I D V
    // - - 1 - - -
    // addressing  assembler  opc  bytes  cycles
    // implied     SEC        38   1      2
    fn op_sec(&mut self, _operand: &Operand) {
        self.state.flags.set(Flags::CARRY, true);
    }

    // Set Decimal Flag
    //
    // 1 -> D
    // N Z C I D V
    // - - - - 1 -
    // addressing  assembler  opc  bytes  cycles
    // implied     SED        F8   1      2
    fn op_sed(&mut self, _operand: &Operand) {
        self.state.flags.set(Flags::DECIMAL, true);
    }

    // Set Interrupt Disable Status
    //
    // 1 -> I
    // N Z C I D V
    // - - - 1 - -
    // addressing  assembler  opc  bytes  cycles
    // implied     SEI        78   1      2
    fn op_sei(&mut self, _operand: &Operand) {
        self.state.flags.set(Flags::INTERRUPTS, true);
    }

    fn op_sha(&mut self, _operand: &Operand) {
        todo!()
    }

    fn op_shx(&mut self, _operand: &Operand) {
        todo!()
    }

    fn op_shy(&mut self, _operand: &Operand) {
        todo!()
    }

    // (ASO)
    //
    // ASL oper + ORA oper
    //
    // M = C <- [76543210] <- 0, A OR M -> A
    // N Z C I D V
    // + + + - - -
    // addressing    assembler     opc  bytes  cycles
    // zeropage      SLO oper      07   2      5
    // zeropage,X    SLO oper,X    17   2      6
    // absolute      SLO oper      0F   3      6
    // absolut,X     SLO oper,X    1F   3      7
    // absolut,Y     SLO oper,Y    1B   3      7
    // (indirect,X)  SLO (oper,X)  03   2      8
    // (indirect),Y  SLO (oper),Y  13   2      8
    fn op_slo(&mut self, operand: &Operand) {
        let (value, _cycles_required) = self.get_operand_value(operand);
        // self.cycles_remaining += cycles_required;

        // ASL
        let result = value.wrapping_shl(1);

        self.flags_from_result(result as u8);
        self.state.flags.set(Flags::CARRY, value & 0x80 != 0);

        self.set_operand_value(operand, result as u8);

        // ORA
        let result = self.state.accumulator | result;
        self.flags_from_result(result);
        self.state.accumulator = result;
    }

    // (LSE)
    //
    // LSR oper + EOR oper
    //
    // M = 0 -> [76543210] -> C, A EOR M -> A
    // N Z C I D V
    // + + + - - -
    // addressing    assembler     opc  bytes  cycles
    // zeropage      SRE oper      47   2      5
    // zeropage,X    SRE oper,X    57   2      6
    // absolute      SRE oper      4F   3      6
    // absolut,X     SRE oper,X    5F   3      7
    // absolut,Y     SRE oper,Y    5B   3      7
    // (indirect,X)  SRE (oper,X)  43   2      8
    // (indirect),Y  SRE (oper),Y  53   2      8
    fn op_sre(&mut self, operand: &Operand) {
        let (value, _cycles_required) = self.get_operand_value(operand);
        // self.cycles_remaining += cycles_required;

        // LSR
        self.state.flags.set(Flags::CARRY, value & 0x01 != 0);
        let result = value.wrapping_shr(1);
        self.flags_from_result(result);

        self.set_operand_value(operand, result);

        // EOR
        let result = self.state.accumulator.bitxor(result);
        self.flags_from_result(result);
        self.state.accumulator = result;
    }

    // Store Accumulator in Memory
    //
    // A -> M
    // N Z C I D V
    // - - - - - -
    // addressing       assembler       opc     bytes   cycles
    // zeropage         STA oper        85      2       3
    // zeropage,X       STA oper,X      95      2       4
    // absolute         STA oper        8D      3       4
    // absolute,X       STA oper,X      9D      3       5
    // absolute,Y       STA oper,Y      99      3       5
    // (indirect,X)     STA (oper,X)    81      2       6
    // (indirect),Y     STA (oper),Y    91      2       6
    fn op_sta(&mut self, operand: &Operand) {
        self.set_operand_value(operand, self.state.accumulator);
    }

    // Store Index X in Memory
    //
    // X -> M
    // N Z C I D V
    // - - - - - -
    // addressing  assembler   opc  bytes  cycles
    // zeropage    STX oper    86   2      3
    // zeropage,Y  STX oper,Y  96   2      4
    // absolute    STX oper    8E   3      4
    fn op_stx(&mut self, operand: &Operand) {
        self.set_operand_value(operand, self.state.x_register);
    }

    // Sore Index Y in Memory
    //
    // Y -> M
    // N Z C I D V
    // - - - - - -
    // addressing   assembler   opc  bytes  cycles
    // zeropage     STY oper    84   2      3
    // zeropage,X   STY oper,X  94   2      4
    // absolute     STY oper    8C   3      4
    fn op_sty(&mut self, operand: &Operand) {
        self.set_operand_value(operand, self.state.y_register);
    }

    fn op_tas(&mut self, _operand: &Operand) {
        todo!()
    }

    // Transfer Accumulator to Index X
    //
    // A -> X
    // N Z C I D V
    // + + - - - -
    // addressing  assembler  opc  bytes  cycles
    // implied     TAX        AA   1      2
    fn op_tax(&mut self, _operand: &Operand) {
        let result = self.state.accumulator;
        self.flags_from_result(result);
        self.state.x_register = result;
    }

    // Transfer Accumulator to Index Y
    //
    // A -> Y
    // N Z C I D V
    // + + - - - -
    // addressing  assembler  opc  bytes  cycles
    // implied     TAY        A8   1      2
    fn op_tay(&mut self, _operand: &Operand) {
        let result = self.state.accumulator;
        self.flags_from_result(result);
        self.state.y_register = result;
    }

    // Transfer Stack Pointer to Index X
    //
    // SP -> X
    // N Z C I D V
    // + + - - - -
    // addressing  assembler  opc  bytes  cycles
    // implied     TSX        BA   1      2
    fn op_tsx(&mut self, _operand: &Operand) {
        let result = self.state.stack_pointer;
        self.flags_from_result(result);
        self.state.x_register = result;
    }

    // Transfer Index X to Accumulator
    //
    // X -> A
    // N Z C I D V
    // + + - - - -
    // addressing  assembler  opc  bytes  cycles
    // implied     TXA        8A   1      2
    fn op_txa(&mut self, _operand: &Operand) {
        let result = self.state.x_register;
        self.flags_from_result(result);
        self.state.accumulator = result;
    }

    // Transfer Index X to Stack Register
    //
    // X -> SP
    // N Z C I D V
    // - - - - - -
    // addressing  assembler  opc  bytes  cycles
    // implied     TXS        9A   1      2
    fn op_txs(&mut self, _operand: &Operand) {
        self.state.stack_pointer = self.state.x_register;
    }

    // Transfer Index Y to Accumulator
    //
    // Y -> A
    // N Z C I D V
    // + + - - - -
    // addressing  assembler  opc  bytes  cycles
    // implied     TYA        98   1      2
    fn op_tya(&mut self, _operand: &Operand) {
        let result = self.state.y_register;
        self.flags_from_result(result);
        self.state.accumulator = result;
    }
}
