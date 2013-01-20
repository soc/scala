abstract class Ab {
   type R   <: AnyRef
   type Rel <: Traversable[R]
   type Corr <: Iterable[R]

   def f1(input: Rel): Corr => Rel
   def f2(input: Rel): Corr => Rel = f1(input)
}

object Conc extends Ab {
  type R = String
  type Rel = List[String]
  type Corr = Set[String]

  def f1(input: List[String]): Set[String] => List[String] = _.toList
}

// object BulkSearchInstance extends BulkSearch {
//        type R   = UpRow
//        type Rel = UpRelation
//        type Corr = UpCorrespondence
// }

// class Row
// class UpRow extends Row

// class Relation [R <: Row]
// class UpRelation extends Relation [UpRow]

// class Correspondence [R <: Row]
// class UpCorrespondence extends Correspondence [UpRow]

// class Mapping[MC <: Correspondence[_]]
