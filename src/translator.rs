use std::collections::HashMap;

use crate::commands::{Address::*, BinaryOp::*, JumpCondition::*, UnaryOp::*, *};

fn base_sym_table() -> HashMap<String, u16> {
    HashMap::from(
        [
            ("SP", 0x0000),
            ("LCL", 0x0001),
            ("ARG", 0x0002),
            ("THIS", 0x0003),
            ("THAT", 0x0004),
            ("R0", 0x0000),
            ("R1", 0x0001),
            ("R2", 0x0002),
            ("R3", 0x0003),
            ("R4", 0x0004),
            ("R5", 0x0005),
            ("R6", 0x0006),
            ("R7", 0x0007),
            ("R8", 0x0008),
            ("R9", 0x0009),
            ("R10", 0x000A),
            ("R11", 0x000B),
            ("R12", 0x000C),
            ("R13", 0x000D),
            ("R14", 0x000E),
            ("R15", 0x000F),
            ("SCREEN", 0x4000),
            ("KBD", 0x8000),
        ]
        .map(|(sym, addr)| (sym.to_string(), addr)),
    )
}

struct SymTable {
    table: HashMap<String, u16>,
    free_var_addr: u16,
}

impl SymTable {
    pub fn new(commands: &Vec<Command>) -> SymTable {
        let mut table = base_sym_table();
        let mut i: u16 = 0;

        for command in commands.iter() {
            if let Command::L(sym) = command {
                let key = sym.clone();
                if table.contains_key(&key) {
                    panic!("Duplicate label symbol {} at instruction {}", key, i);
                }
                table.insert(key, i);
            } else {
                i += 1;
            }
        }

        SymTable {
            table,
            free_var_addr: 0x0010,
        }
    }

    fn insert(&mut self, sym: &String) -> u16 {
        let addr = self.free_var_addr;
        self.table.insert(sym.clone(), addr);
        self.free_var_addr += 1;
        addr
    }

    pub fn lookup(&mut self, sym: &String) -> u16 {
        self.table
            .get(sym)
            .map(|a| *a)
            .unwrap_or_else(|| self.insert(sym))
    }
}

fn translate_a(address: &Address, sym_table: &mut SymTable) -> u16 {
    match address {
        Constant(addr) => *addr,
        Symbol(sym) => sym_table.lookup(sym),
    }
}

fn mask_if(b: bool, mask: u16) -> u16 {
    if b {
        mask
    } else {
        0x0000
    }
}

fn translate_c(dest: &Destination, expr: &Expression, jump: &Option<JumpCondition>) -> u16 {
    let mut instruction = 0xE000;

    instruction |= mask_if(dest.a, 0x0020);
    instruction |= mask_if(dest.d, 0x0010);
    instruction |= mask_if(dest.m, 0x0008);

    instruction |= match jump {
        None => 0x0000,
        Some(j) => match j {
            Greater => 0x0001,
            Equal => 0x0002,
            GreaterEqual => 0x0003,
            Less => 0x0004,
            NotEqual => 0x0005,
            LessEqual => 0x0006,
            Unconditional => 0x0007,
        },
    };

    instruction |= match expr {
        Expression::Unary(op, rhs) => match (op, rhs) {
            (Identity, Rhs::Zero) => 0x0A80,
            (Identity, Rhs::One) => 0x0FC0,
            (Identity, Rhs::A) => 0x0C00,
            (Identity, Rhs::D) => 0x0300,
            (Identity, Rhs::M) => 0x1C00,
            (Negate, Rhs::Zero) => 0x0A80,
            (Negate, Rhs::One) => 0x0E80,
            (Negate, Rhs::A) => 0x0CC0,
            (Negate, Rhs::D) => 0x03C0,
            (Negate, Rhs::M) => 0x1CC0,
            (Not, Rhs::Zero) => panic!("'!0' not permitted"),
            (Not, Rhs::One) => panic!("'!1' not permitted"),
            (Not, Rhs::A) => 0x0C40,
            (Not, Rhs::D) => 0x0340,
            (Not, Rhs::M) => 0x1C40,
        },
        Expression::Binary(lhs, op, rhs) => match (lhs, op, rhs) {
            (Lhs::A, _, Rhs::M) => panic!("Should not accept A and M in the same clause"),
            (Lhs::M, _, Rhs::A) => panic!("Should not accept A and M in the same clause"),
            (Lhs::A, _, Rhs::A) => panic!("Should not accept same LHS and RHS"),
            (Lhs::D, _, Rhs::D) => panic!("Should not accept same LHS and RHS"),
            (Lhs::M, _, Rhs::M) => panic!("Should not accept same LHS and RHS"),
            (_, _, Rhs::Zero) => panic!("Should not use zero in binary ops"),
            (_, And | Or, Rhs::One) => panic!("Should not use one as argument to and/or"),

            (Lhs::A, Add, Rhs::One) => 0x0DC0,
            (Lhs::D, Add, Rhs::One) => 0x07C0,
            (Lhs::M, Add, Rhs::One) => 0x1DC0,
            (Lhs::A | Lhs::D, Add, Rhs::A | Rhs::D) => 0x0080,
            (Lhs::D | Lhs::M, Add, Rhs::D | Rhs::M) => 0x1080,
            (Lhs::A, Subtract, Rhs::One) => 0x0C80,
            (Lhs::D, Subtract, Rhs::One) => 0x0380,
            (Lhs::M, Subtract, Rhs::One) => 0x1C80,
            (Lhs::A, Subtract, Rhs::D) => 0x01C0,
            (Lhs::D, Subtract, Rhs::A) => 0x04C0,
            (Lhs::D, Subtract, Rhs::M) => 0x14C0,
            (Lhs::M, Subtract, Rhs::D) => 0x11C0,
            (Lhs::A | Lhs::D, And, Rhs::A | Rhs::D) => 0x0000,
            (Lhs::D | Lhs::M, And, Rhs::D | Rhs::M) => 0x1000,
            (Lhs::A | Lhs::D, Or, Rhs::A | Rhs::D) => 0x0540,
            (Lhs::D | Lhs::M, Or, Rhs::D | Rhs::M) => 0x1540,
        },
    };

    instruction
}

pub fn translate(commands: &Vec<Command>) -> Vec<u16> {
    let mut sym_table = SymTable::new(commands);
    let mut instructions: Vec<u16> = vec![];

    for command in commands {
        let instruction = match command {
            Command::L(_) => continue,
            Command::A(addr) => translate_a(addr, &mut sym_table),
            Command::C(dest, expr, jump) => translate_c(dest, expr, jump),
        };

        instructions.push(instruction);
    }

    instructions
}
