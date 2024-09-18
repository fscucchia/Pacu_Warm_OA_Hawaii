### High v Control 

BP

```
go.obo High_v_Control_gene_to_go.tab DE_High_v_Control_LFC.csv BP largest=0.5 smallest=5 cutHeight=0.25

Run parameters:

largest GO category as fraction of all genes (largest)  : 0.5
         smallest GO category as # of genes (smallest)  : 5
                clustering threshold (clusterCutHeight) : 0.25

-----------------
retrieving GO hierarchy, reformatting data...

-------------
go_reformat:
Genes with GO annotations, but not listed in measure table: 1

Terms without defined level (old ontology?..): 894
-------------
-------------
go_nrify:
14519 categories, 7913 genes; size range 5-3956.5
        11 too broad
        6121 too small
        8387 remaining

removing redundancy:

calculating GO term similarities based on shared genes...
5866 non-redundant GO categories of good size
-------------

Secondary clustering:
calculating similarities....
Continuous measure of interest: will perform MWU test
583 GO terms at 10% FDR

     delta.rank         pval level nseqs       term
574        1305 2.114365e-40     2   586 GO:0006259
785        -994 7.992417e-05     3    83 GO:0006767
2060      -1616 2.882699e-06     2    44 GO:0022617
3359       -633 5.533561e-21     2  1396 GO:0044281
3956       -501 5.544711e-08     2   670 GO:0048878
4693      -2168 2.156610e-06     4    25 GO:0070286
                                        name        p.adj
574                    DNA metabolic process 6.200377e-37
785  water-soluble vitamin metabolic process 2.332116e-03
2060        extracellular matrix disassembly 1.207645e-04
3359        small molecule metabolic process 2.950394e-18
3956                    chemical homeostasis 3.534753e-06
4693        axonemal dynein complex assembly 9.655357e-05
```

CC 

```
go.obo High_v_Control_gene_to_go.tab DE_High_v_Control_LFC.csv CC largest=0.5 smallest=5 cutHeight=0.25

bestGOs
```

MF

```
go.obo High_v_Control_gene_to_go.tab DE_High_v_Control_LFC.csv MF largest=0.5 smallest=5 cutHeight=0.25

bestGOs
```

### High v Mid 

BP

```
go.obo High_v_Mid_gene_to_go.tab DE_High_v_Mid_LFC.csv BP largest=0.5 smallest=5 cutHeight=0.25

Run parameters:

largest GO category as fraction of all genes (largest)  : 0.5
         smallest GO category as # of genes (smallest)  : 5
                clustering threshold (clusterCutHeight) : 0.25

-----------------
retrieving GO hierarchy, reformatting data...

-------------
go_reformat:
Genes with GO annotations, but not listed in measure table: 1

Terms without defined level (old ontology?..): 895
-------------
-------------
go_nrify:
14554 categories, 8177 genes; size range 5-4088.5
        12 too broad
        6091 too small
        8451 remaining

removing redundancy:

calculating GO term similarities based on shared genes...
5903 non-redundant GO categories of good size
-------------

Secondary clustering:
calculating similarities....
Continuous measure of interest: will perform MWU test
366 GO terms at 10% FDR

     delta.rank         pval level nseqs                             term
683       -2160 2.564696e-04     4    16                       GO:0006509
813        1360 5.491615e-08     2    90 GO:0006821;GO:0098661;GO:1902476
2871       -874 1.230452e-04     4   109                       GO:0035270
3392       -684 2.645318e-23     2  1428                       GO:0044281
3986       -389 2.928228e-05     2   704                       GO:0048878
4721      -2062 1.294959e-05     4    25                       GO:0070286
5071        736 2.395887e-31     2  1782                       GO:0090304
                                        name        p.adj
683  membrane protein ectodomain proteolysis 9.893358e-03
813  inorganic anion transmembrane transport 8.102878e-06
2871            endocrine system development 5.407911e-03
3392        small molecule metabolic process 7.806334e-20
3986                    chemical homeostasis 1.745697e-03
4721        axonemal dynein complex assembly 8.991585e-04
5071          nucleic acid metabolic process 1.414053e-27
```

CC 

```
go.obo High_v_Mid_gene_to_go.tab DE_High_v_Mid_LFC.csv CC largest=0.5 smallest=5 cutHeight=0.25

    delta.rank         pval level nseqs                             term                                              name        p.adj
```

MF

```
go.obo High_v_Mid_gene_to_go.tab DE_High_v_Mid_LFC.csv MF largest=0.5 smallest=5 cutHeight=0.25

     delta.rank         pval level nseqs       term                                                   name        p.adj
```

### Mid v Control

BP

```
go.obo Mid_v_Control_gene_to_go.tab DE_Mid_v_Control_LFC.csv BP largest=0.5 smallest=5 cutHeight=0.25

Run parameters:

largest GO category as fraction of all genes (largest)  : 0.5
         smallest GO category as # of genes (smallest)  : 5
                clustering threshold (clusterCutHeight) : 0.25

-----------------
retrieving GO hierarchy, reformatting data...

-------------
go_reformat:
Genes with GO annotations, but not listed in measure table: 1

Terms without defined level (old ontology?..): 895
-------------
-------------
go_nrify:
14566 categories, 8224 genes; size range 5-4112
        12 too broad
        6073 too small
        8481 remaining

removing redundancy:

calculating GO term similarities based on shared genes...
5918 non-redundant GO categories of good size
-------------
Secondary clustering:
calculating similarities....
Continuous measure of interest: will perform MWU test
551 GO terms at 10% FDR

     delta.rank         pval level nseqs                             term
1893      -2814 8.546494e-05     2    11                       GO:0018146
2070        789 2.514432e-21     2   915            GO:0022402;GO:0007049
3655      -1136 2.248450e-08     4   139                       GO:0046034
5192      -1048 1.261418e-19     3   446 GO:0098655;GO:0098662;GO:0098660
5384        709 2.563127e-05     2   204 GO:1901605;GO:0170033;GO:0170039
                                      name        p.adj
1893  keratan sulfate biosynthetic process 2.906299e-03
2070                            cell cycle 5.142043e-18
3655                 ATP metabolic process 2.608643e-06
5192 inorganic ion transmembrane transport 1.243969e-16
5384    alpha-amino acid metabolic process 1.038769e-03
```

CC

```
go.obo Mid_v_Control_gene_to_go.tab DE_Mid_v_Control_LFC.csv CC largest=0.5 smallest=5 cutHeight=0.25

    delta.rank         pval level nseqs       term     name        p.adj
```

MF

```
go.obo Mid_v_Control_gene_to_go.tab DE_Mid_v_Control_LFC.csv MF largest=0.5 smallest=5 cutHeight=0.25

     delta.rank         pval level nseqs                  term                                              name        p.adj
```