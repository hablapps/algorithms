package CYK

sealed abstract class Production
case class UnitProduction(generator: NonTerminal, generated: Terminal) extends Production
case class SymbolProduction(generator: NonTerminal, gen1: NonTerminal, gen2: NonTerminal) extends Production
