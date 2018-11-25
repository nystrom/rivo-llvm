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
}

impl ProcTranslator {
    fn new() -> Self {
        ProcTranslator {
        }
    }

    fn new_temp(&mut self) -> Name {
        Name::fresh("t.lir")
    }

    fn new_label(&mut self) -> Name {
        Name::fresh("L.lir")
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
            mir::Stm::CJump { cond, if_true, if_false } => {
                let t = self.new_temp();

                let mut ss = self.translate_exp(t, &*cond);
                ss.push(
                    lir::Stm::CJump {
                        cmp: lir::Exp::Temp { name: t },
                        if_true: *if_true,
                        if_false: *if_false,
                    }
                );
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
            mir::Stm::Return { exp } => {
                let t = self.new_temp();
                let mut ss = self.translate_exp(t, &*exp);
                ss.push(
                    lir::Stm::Ret {
                        exp: lir::Exp::Temp { name: t }
                    }
                );
                ss
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
            mir::Exp::Call { ret_type, fun, args } => {
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
            mir::Exp::GetStructElementAddr { struct_ty, ptr, field } => {
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
            mir::Exp::GetArrayElementAddr { base_ty, ptr, index } => {
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
            mir::Exp::GetArrayLengthAddr { ptr } => {
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
            mir::Exp::Cast { ty, exp } => {
                let t = self.new_temp();
                ss.append(&mut self.translate_exp(t, &*exp));
                ss.push(
                    lir::Stm::Cast {
                        dst,
                        ty: ty.clone(),
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
            mir::Exp::Global { name, ty } => {
                ss.push(
                    lir::Stm::Move {
                        dst,
                        src: lir::Exp::Global { name: *name }
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
