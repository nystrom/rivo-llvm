use crate::mir::trees as mir;
use crate::lir::trees as lir;
use crate::hir::ops::*;
use crate::common::names::*;


pub struct Translate;

impl Translate {
    pub fn translate(r: &mir::Root) -> lir::Root {
        let procs = r.procs.iter().map(|p| ProcTranslator::new().translate_proc(p)).collect();
        lir::Root { data: vec![], procs }
    }
}

struct ProcTranslator {
    next_temp: u32,
    next_label: u32,
}

impl ProcTranslator {
    fn new() -> Self {
        ProcTranslator {
            next_temp: 0,
            next_label: 0,
        }
    }

    fn new_temp(&mut self) -> Name {
        let t = format!("t.lir.{}", self.next_temp);
        self.next_temp += 1;
        Name::new(&t)
    }

    fn new_label(&mut self) -> Name {
        let t = format!("L.lir.{}", self.next_label);
        self.next_label += 1;
        Name::new(&t)
    }

    fn translate_proc(&mut self, p: &mir::Proc) -> lir::Proc {
        let t = self.new_temp();
        let mut ss = self.translate_exp(t, &p.body);
        ss.push(lir::Stm::Ret { exp: lir::Exp::Temp { name: t } });

        lir::Proc {
            ty: p.ty.clone(),
            name: p.name.clone(),
            params: p.params.clone(),
            body: ss,
        }
    }

    fn translate_stm(&mut self, e: &mir::Stm) -> Vec<lir::Stm> {
        match e {
            mir::Stm::Nop => {
                vec![
                    lir::Stm::Nop
                ]
            },
            mir::Stm::Error { message } => {
                unimplemented!()
            },
            mir::Stm::CJump { cond, if_true, if_false } => {
                let t1 = self.new_temp();
                let t2 = self.new_temp();
                let mut ss = Vec::new();

                match &**cond {
                    mir::Exp::Binary { op: Bop::Eq_i32, e1, e2 } => {
                        let mut ss1 = self.translate_exp(t1, &*e1);
                        let mut ss2 = self.translate_exp(t2, &*e2);

                        ss.append(&mut ss1);
                        ss.append(&mut ss2);
                        ss.push(
                            lir::Stm::CJump {
                                cmp: lir::Cmp::Eq_i32,
                                e1: lir::Exp::Temp { name: t1 },
                                e2: lir::Exp::Temp { name: t2 },
                                if_true: *if_true,
                                if_false: *if_false,
                            }
                        );
                    },
                    mir::Exp::Binary { op: Bop::Ne_i32, e1, e2 } => {
                        let mut ss1 = self.translate_exp(t1, &*e1);
                        let mut ss2 = self.translate_exp(t2, &*e2);

                        ss.append(&mut ss1);
                        ss.append(&mut ss2);
                        ss.push(
                            lir::Stm::CJump {
                                cmp: lir::Cmp::Ne_i32,
                                e1: lir::Exp::Temp { name: t1 },
                                e2: lir::Exp::Temp { name: t2 },
                                if_true: *if_true,
                                if_false: *if_false,
                            }
                        );
                    },
                    cond => {
                        let mut ss1 = self.translate_exp(t1, &*cond);
                        ss.append(&mut ss1);
                        ss.push(
                            lir::Stm::CJump {
                                cmp: lir::Cmp::Ne_i32,
                                e1: lir::Exp::Temp { name: t1 },
                                e2: lir::Exp::Lit { lit: mir::Lit::I32 { value: 0 } },
                                if_true: *if_true,
                                if_false: *if_false,
                            }
                        );
                    },
                }
                ss
            },
            mir::Stm::Jump { label } => {
                vec![
                    lir::Stm::Jump { label: *label }
                ]
            },
            mir::Stm::Label { label } => {
                vec![
                    lir::Stm::Label { label: *label }
                ]
            },
            mir::Stm::Move { ty, lhs, rhs } => {
                self.translate_exp(*lhs, rhs)
            },
            mir::Stm::Store { ty, ptr, value } => {
                let p = self.new_temp();
                let v = self.new_temp();
                let mut ssp = self.translate_exp(p, ptr);
                let mut ssv = self.translate_exp(v, value);
                let mut ss = Vec::new();
                ss.append(&mut ssp);
                ss.append(&mut ssv);
                ss.push(
                    lir::Stm::Store {
                        dst_addr: lir::Exp::Temp { name: p },
                        src: lir::Exp::Temp { name: v },
                    }
                );
                ss
            },
        }
    }

    fn translate_exp(&mut self, dst: Name, e: &mir::Exp) -> Vec<lir::Stm> {
        let mut ss = Vec::new();
        match e {
            mir::Exp::Block { body, exp } => {
                for s in body {
                    ss.append(&mut self.translate_stm(s));
                }
                ss.append(&mut self.translate_exp(dst, &*exp));
            },
            mir::Exp::Call { fun, args } => {
                let mut arg_regs = Vec::new();
                let f = self.new_temp();
                ss.append(&mut self.translate_exp(f, &*fun));
                for arg in args {
                    let t = self.new_temp();
                    ss.append(&mut self.translate_exp(t, &*arg));
                    arg_regs.push(lir::Exp::Temp { name: t });
                }
                ss.push(
                    lir::Stm::Call {
                        dst,
                        fun: lir::Exp::Temp { name: f },
                        args: arg_regs,
                    }
                );
            },
            mir::Exp::StructAlloc { ty } => {
                unimplemented!()
            },
            mir::Exp::ArrayAlloc { base_ty, length } => {
                unimplemented!()
            },
            mir::Exp::StructAddr { struct_ty, ptr, field } => {
                let t = self.new_temp();
                ss.append(&mut self.translate_exp(t, &*ptr));
                ss.push(
                    lir::Stm::GetStructElementAddr {
                        dst,
                        struct_ty: struct_ty.clone(),
                        ptr: lir::Exp::Temp { name: t },
                        field: *field
                    }
                );
            },
            mir::Exp::ArrayAddr { base_ty, ptr, index } => {
                let a = self.new_temp();
                let i = self.new_temp();
                ss.append(&mut self.translate_exp(a, &*ptr));
                ss.append(&mut self.translate_exp(i, &*index));
                ss.push(
                    lir::Stm::GetArrayElementAddr {
                        dst,
                        base_ty: base_ty.clone(),
                        ptr: lir::Exp::Temp { name: a },
                        index: lir::Exp::Temp { name: i },
                    }
                );
            },
            mir::Exp::ArrayLengthAddr { ptr } => {
                let t = self.new_temp();
                ss.append(&mut self.translate_exp(t, &*ptr));
                ss.push(
                    lir::Stm::GetArrayLengthAddr {
                        dst,
                        ptr: lir::Exp::Temp { name: t }
                    }
                );
            },
            mir::Exp::Load { ty, ptr } => {
                let t = self.new_temp();
                ss.append(&mut self.translate_exp(t, &*ptr));
                ss.push(
                    lir::Stm::Load {
                        dst,
                        src_addr: lir::Exp::Temp { name: t }
                    }
                );
            },
            mir::Exp::Binary { op, e1, e2 } => {
                let t1 = self.new_temp();
                let t2 = self.new_temp();
                ss.append(&mut self.translate_exp(t1, &*e1));
                ss.append(&mut self.translate_exp(t2, &*e2));
                ss.push(
                    lir::Stm::Binary {
                        dst,
                        op: *op,
                        e1: lir::Exp::Temp { name: t1 },
                        e2: lir::Exp::Temp { name: t2 },
                    }
                );
            },
            mir::Exp::Unary { op, exp } => {
                let t = self.new_temp();
                ss.append(&mut self.translate_exp(t, &*exp));
                ss.push(
                    lir::Stm::Unary {
                        dst,
                        op: *op,
                        exp: lir::Exp::Temp { name: t }
                    }
                );
            },
            mir::Exp::Lit { lit } => {
                ss.push(
                    lir::Stm::Move {
                        dst,
                        src: lir::Exp::Lit { lit: lit.clone() }
                    }
                );
            },
            mir::Exp::Global { label, ty } => {
                ss.push(
                    lir::Stm::Move {
                        dst,
                        src: lir::Exp::Global { name: *label }
                    }
                );
            },
            mir::Exp::Temp { name, ty } => {
                ss.push(
                    lir::Stm::Move {
                        dst,
                        src: lir::Exp::Temp { name: *name }
                    }
                );
            },
        }
        ss
    }
}
