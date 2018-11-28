use crate::mir::trees as mir;
use crate::mir::typed::*;
use crate::lir::typed::*;
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
        let mut ss = Vec::new();
        let t = self.translate_exp_into(&p.body, &mut ss);
        ss.push(lir::Stm::Ret { exp: t });

        lir::Proc {
            ret_type: p.ret_type.clone(),
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
                let mut ss = Vec::new();
                let t = self.translate_exp_into(&*cond, &mut ss);
                ss.push(
                    lir::Stm::CJump {
                        cmp: t,
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
                let mut ss = Vec::new();
                let t = self.translate_exp_into(&*rhs, &mut ss);
                ss.push(
                    lir::Stm::Move {
                        dst: lir::Exp::Temp { ty: ty.clone(), name: *lhs },
                        src: t,
                    }
                );
                ss
            },
            mir::Stm::Return { exp } => {
                let mut ss = Vec::new();
                let t = self.translate_exp_into(&*exp, &mut ss);
                ss.push(
                    lir::Stm::Ret {
                        exp: t
                    }
                );
                ss
            },
            mir::Stm::Store { ty, ptr, value } => {
                let mut ss = Vec::new();
                let p = self.translate_exp_into(&*ptr, &mut ss);
                let v = self.translate_exp_into(&*value, &mut ss);

                ss.push(
                    lir::Stm::Store {
                        dst_addr: p,
                        src: v,
                    }
                );
                ss
            },
        }
    }

    fn translate_exp_into(&mut self, e: &mir::Exp, ss: &mut Vec<lir::Stm>) -> lir::Exp {
        let dst_ty = e.get_type();
        let dst = lir::Exp::Temp { ty: dst_ty, name: self.new_temp() };

        match e {
            mir::Exp::Block { body, exp } => {
                for s in body {
                    ss.append(&mut self.translate_stm(s));
                }
                self.translate_exp_into(&*exp, ss)
            },
            mir::Exp::Call { fun_type, fun, args } => {
                let f = self.translate_exp_into(&*fun, ss);

                let mut arg_regs = Vec::new();
                for arg in args {
                    let t = self.translate_exp_into(&*arg, ss);
                    arg_regs.push(t);
                }

                ss.push(
                    lir::Stm::Call {
                        dst: dst.clone(),
                        fun: f,
                        args: arg_regs,
                    }
                );

                dst
            },
            mir::Exp::GetStructElementAddr { struct_ty, ptr, field } => {
                let p = self.translate_exp_into(&*ptr, ss);

                ss.push(
                    lir::Stm::GetStructElementAddr {
                        dst: dst.clone(),
                        struct_ty: struct_ty.clone(),
                        ptr: p,
                        field: *field
                    }
                );

                dst
            },
            mir::Exp::GetArrayElementAddr { base_ty, ptr, index } => {
                let p = self.translate_exp_into(&*ptr, ss);
                let i = self.translate_exp_into(&*index, ss);

                ss.push(
                    lir::Stm::GetArrayElementAddr {
                        dst: dst.clone(),
                        base_ty: base_ty.clone(),
                        ptr: p,
                        index: i,
                    }
                );

                dst
            },
            mir::Exp::GetArrayLengthAddr { ptr } => {
                let p = self.translate_exp_into(&*ptr, ss);

                ss.push(
                    lir::Stm::GetArrayLengthAddr {
                        dst: dst.clone(),
                        ptr: p,
                    }
                );

                dst
            },
            mir::Exp::Load { ty, ptr } => {
                let p = self.translate_exp_into(&*ptr, ss);

                ss.push(
                    lir::Stm::Load {
                        dst: dst.clone(),
                        src_addr: p,
                    }
                );

                dst
            },
            mir::Exp::Binary { op, e1, e2 } => {
                let t1 = self.translate_exp_into(&*e1, ss);
                let t2 = self.translate_exp_into(&*e2, ss);

                ss.push(
                    lir::Stm::Binary {
                        dst: dst.clone(),
                        op: *op,
                        e1: t1,
                        e2: t2,
                    }
                );

                dst
            },
            mir::Exp::Unary { op, exp } => {
                let t = self.translate_exp_into(&*exp, ss);

                ss.push(
                    lir::Stm::Unary {
                        dst: dst.clone(),
                        op: *op,
                        exp: t,
                    }
                );

                dst
            },
            mir::Exp::Cast { ty, exp } => {
                let t = self.translate_exp_into(&*exp, ss);

                ss.push(
                    lir::Stm::Cast {
                        dst: dst.clone(),
                        ty: ty.clone(),
                        exp: t,
                    }
                );

                dst
            },
            mir::Exp::Lit { lit } => {
                lir::Exp::Lit { lit: lit.clone() }
            },
            mir::Exp::Global { name, ty } => {
                lir::Exp::Global { name: *name, ty: ty.clone() }
            },
            mir::Exp::Function { name, ty } => {
                lir::Exp::Function { name: *name, ty: ty.clone() }
            },
            mir::Exp::Temp { name, ty } => {
                lir::Exp::Temp { name: *name, ty: ty.clone() }
            },
        }
    }
}
