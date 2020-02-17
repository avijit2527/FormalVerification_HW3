package twiddlebits

abstract class Expr {

    // bit-wise logical operators
    def unary_!(): BV_NOT = BV_NOT(this)
    def &(other: Expr): BV_AND = BV_AND(this, other)
    def |(other: Expr): BV_OR = BV_OR(this, other)
    def ^(other: Expr): BV_XOR = BV_XOR(this, other)
    def <<<(other: Expr): BV_LSHIFT = BV_LSHIFT(this, other)
    def >>>(other: Expr): BV_RSHIFT = BV_RSHIFT(this, other)

    // arithmetic operators
    def +(other: Expr): BV_ADD = BV_ADD(this, other)
    def -(other: Expr): BV_SUB = BV_SUB(this, other)
    def *(other: Expr): BV_MUL = BV_MUL(this, other)
    def /(other: Expr): BV_DIV = BV_DIV(this, other)


    // relational operators
    def ==(other:Expr): BV_EQ = BV_EQ(this, other)

}


case class BV_EQ(lexpr: Expr, rexpr: Expr) extends Expr {
    override def toString: String = lexpr.toString + " == " + rexpr.toString
}


case class BV_VAR(name: String) extends Expr {
    override def toString: String = name
}

case class BV_NUM(value: Int) extends Expr {
    override def toString: String = "bv_const_" + value.toString
}

case class BV_NOT(expr: Expr) extends Expr {
    override def toString: String = "!(" + expr.toString + ")"
}

case class BV_RSHIFT(lexpr: Expr, rexpr: Expr) extends Expr {
    override def toString: String = "(" + lexpr.toString + " >>> " + rexpr.toString +")"
}

case class BV_LSHIFT(lexpr: Expr, rexpr: Expr) extends Expr {
    override def toString: String = "(" + lexpr.toString + " <<< " + rexpr.toString +")"
}

case class BV_AND(lexpr: Expr, rexpr: Expr) extends Expr {
    override def toString: String = "(" + lexpr.toString + " & " + rexpr.toString +")"
}

case class BV_OR(lexpr: Expr, rexpr: Expr)  extends Expr {
    override def toString: String = "(" + lexpr.toString + " | " + rexpr.toString +")"
}

case class BV_XOR(lexpr: Expr, rexpr: Expr)  extends Expr {
    override def toString: String = "(" + lexpr.toString + " ^ " + rexpr.toString +")"
}

case class BV_ADD(lexpr: Expr, rexpr: Expr)  extends Expr {
    override def toString: String = "(" + lexpr.toString + " + " + rexpr.toString +")"
}

case class BV_SUB(lexpr: Expr, rexpr: Expr)  extends Expr {
    override def toString: String = "(" + lexpr.toString + " - " + rexpr.toString +")"
}

case class BV_MUL(lexpr: Expr, rexpr: Expr)  extends Expr {
    override def toString: String = "(" + lexpr.toString + " * " + rexpr.toString +")"
}

case class BV_DIV(lexpr: Expr, rexpr: Expr)  extends Expr {
    override def toString: String = "(" + lexpr.toString + " / " + rexpr.toString +")"
}


