# Programmatic querying of the Semantic MEDLINE database

An R package that provides tools for differential analysis in MPRA studies.

The Semantic MEDLINE database (SemMedDB) is a collection of annotations of sentences from the abstracts of articles indexed in PubMed. These annotations take the form of subject-predicate-object triples of information. These triples are also called **predications**.

An example predication is "Interleukin-12 INTERACTS_WITH IFNA1". Here, the subject is "Interleukin-12", the object is "IFNA1" (interferon alpha-1), and the predicate linking the subject and object is "INTERACTS_WITH". The Semantic MEDLINE database consists of tens of millions of these predications.

The predications in SemMedDB can be represented in graph form. Nodes represent concepts, and directed edges represent predicates (concept linkers). In particular, the Semantic MEDLINE graph is a directed **multigraph** because multiple predicates are often present between pairs of nodes (e.g., "A ASSOCIATED_WITH B" and "A INTERACTS_WITH B"). `rsemmed` relies on the `igraph` package for efficient graph operations.

The full processed graph representation is available [here](https://drive.google.com/file/d/1b2Drq_NktFmbCBUQHlVjmRG85u4OsavR/view?usp=sharing). It is a processed version of the PREDICATION table (a SQL dump file) available from the [National Library of Medicine site](https://skr3.nlm.nih.gov/SemMedDB/index.html) for Semantic MEDLINE. See the package vignette for details about the processing.
