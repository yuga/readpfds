include ./ocaml_basic.gmk

test::
	ocaml < runTest.ml

all:: smallStream.cmo ordered.cmo \
      rqueue.cmo realTimeQueue.cmo \
      heap.cmo scheduledBinomialHeap.cmo \
      sortable.cmo scheduledBottomUpMergeSort.cmo

