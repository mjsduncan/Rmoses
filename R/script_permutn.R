i'm copying a chat i started with eddie to open this up for ideas before i get too far on a potentially incorrect path (edited by mike for clarity & continuity):

re protein & gene nodes, is there any other structure associated with either one of them besides being a name-able type?

the only pre-defined structure for gene and protein nodes is that they inherit from ConceptNode
in general, ben has discouraged creation of new atom types and instead use predicates or inheritance. he thought that gene and proteins would have occur often enough to warrant their own atom types though

working on the pathway database import it does seem useful, although the names "protein" & "gene" should really be a little more general like DNAtemplate and product...

i'm going to try protein nodes for the pathway nodes and predicates for the links between them and call the set of particular links & nodes a concept node of the pathway name.
not sure what that will mean when in the atomspace with the go network.  i guess there should be a predicate link between them (the GeneNode members of a GO ConceptNode and the ProteinNodes in a pathway ConceptNode) to represent instantiation?

or something else?

you could use a similarity link, but i think doing a predicate is better
or maybe both or have a domain-particular rule that creates the similarity link based on the predicate

the idea of template and product and their relationship seems like a more general conceptual structure that gene-protein is an example of...

there could be those Concepts that can be inherited from or related in some other way
what are you thinking here regarding how to represent the relationship between gene and protein?
what's leading you to say that?

i guess the notion of generality and not making too many specific types.  there are specific types for spatial and temporal relations, maybe there should be general types for process.  isn't that [procedural] a particular type of memory structure?

or because there are DNAtemplates in pathways besides just genes and the same for products / proteins

there are other products of dna templates besides proteins.  it's kind of a quible to say a miRNA template isn't a gene.  but genes are made of exon substrings that can be mixed & matched.  & there are dna strings that match with regulatory proteins that turn genes on and off...

if something is not considered a gene in the field, then it shouldn't be represented by a GeneNode imo
i think these constructs need to be represented accurately. a question is whether to represent something by creating a new atom type vs using relations in the atomspace using the existing atom types. types can be created through relations in the atomspace or by creating a new atom type. i am not particularly clear what the advantages and disadvantages of each are
one reason against creating a lot of particular atom types is that the pln rules only deal with the core atom types
and the ure/pattern matcher don't yet handle atom type inheritance
so creating specialized atom types could definitely get in the way of pln inference

