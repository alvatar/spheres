(%load-library '(spheres/util test))

(%load-library '(spheres/algorithm topological-sort))

(test-begin "topologica-sort" 1)

(test-equal "topological sort"
            (topological-sort '((shirt tie belt)
                                (tie jacket)
                                (belt jacket)
                                (watch)
                                (pants shoes belt)
                                (undershorts pants shoes)
                                (socks shoes))
                              eq?)
            '(socks undershorts pants shoes watch shirt belt tie jacket))

(test-end)
