(##spheres-load energy: testing)

(include "src/u8.scm")

(test-begin "u8 Strings")

(test-equal "u8vector-normalize-eol:s 1a"
            (u8vector-normalize-eol:s '#u8(13) 'cr)
 '#u8(13))

(test-equal "u8vector-normalize-eol:s 1b"
            (u8vector-normalize-eol:s '#u8(10) 'cr)
 '#u8(13))

(test-equal "u8vector-normalize-eol:s 1c"
            (u8vector-normalize-eol:s '#u8(13 10) 'cr)
 '#u8(13))

(test-equal "u8vector-normalize-eol:s 2a"
            (u8vector-normalize-eol:s '#u8(13 10) 'lf)
 '#u8(10))

(test-equal "u8vector-normalize-eol:s 2b"
            (u8vector-normalize-eol:s '#u8(13 10) 'lf)
 '#u8(10))

(test-equal "u8vector-normalize-eol:s 2c"
            (u8vector-normalize-eol:s '#u8(13 10) 'lf)
 '#u8(10))

(test-equal "u8vector-normalize-eol:s 3a"
            (u8vector-normalize-eol:s '#u8(13) 'cr-lf)
 '#u8(13 10))

(test-equal "u8vector-normalize-eol:s 3b"
            (u8vector-normalize-eol:s '#u8(10) 'cr-lf)
 '#u8(13 10))

(test-equal "u8vector-normalize-eol:s 3c"
            (u8vector-normalize-eol:s '#u8(13 10) 'cr-lf)
 '#u8(13 10))

(test-equal "u8vector-normalize-eol:s 4a"
            (u8vector-normalize-eol:s '#u8(13 1) 'cr)
 '#u8(13 1))

(test-equal "u8vector-normalize-eol:s 4b"
            (u8vector-normalize-eol:s '#u8(10 1) 'cr)
 '#u8(13 1))

(test-equal "u8vector-normalize-eol:s 4c"
            (u8vector-normalize-eol:s '#u8(13 10 1) 'cr)
 '#u8(13 1))

(test-equal "u8vector-normalize-eol:s 5a"
            (u8vector-normalize-eol:s '#u8(13 1) 'lf)
 '#u8(10 1))

(test-equal "u8vector-normalize-eol:s 5b"
            (u8vector-normalize-eol:s '#u8(10 1) 'lf)
 '#u8(10 1))

(test-equal "u8vector-normalize-eol:s 5c"
            (u8vector-normalize-eol:s '#u8(13 10 1) 'lf)
 '#u8(10 1))

(test-equal "u8vector-normalize-eol:s 6a"
            (u8vector-normalize-eol:s '#u8(13 1) 'cr-lf)
 '#u8(13 10 1))

(test-equal "u8vector-normalize-eol:s 6b"
            (u8vector-normalize-eol:s '#u8(10 1) 'cr-lf)
 '#u8(13 10 1))

(test-equal "u8vector-normalize-eol:s 6c"
            (u8vector-normalize-eol:s '#u8(13 10 1) 'cr-lf)
 '#u8(13 10 1))

(test-equal "u8vector-normalize-eol:s 7a"
            (u8vector-normalize-eol:s '#u8(1 13) 'cr)
 '#u8(1 13))

(test-equal "u8vector-normalize-eol:s 7b"
            (u8vector-normalize-eol:s '#u8(1 10) 'cr)
 '#u8(1 13))

(test-equal "u8vector-normalize-eol:s 7c"
            (u8vector-normalize-eol:s '#u8(1 13 10) 'cr)
 '#u8(1 13))

(test-equal "u8vector-normalize-eol:s 8a"
            (u8vector-normalize-eol:s '#u8(1 13) 'lf)
            '#u8(1 10))

(test-equal "u8vector-normalize-eol:s 8b"
            (u8vector-normalize-eol:s '#u8(1 10) 'lf)
            '#u8(1 10))

(test-equal "u8vector-normalize-eol:s 8c"
            (u8vector-normalize-eol:s '#u8(1 13 10) 'lf)
            '#u8(1 10))

(test-equal "u8vector-normalize-eol:s 9a"
            (u8vector-normalize-eol:s '#u8(1 13) 'cr-lf)
            '#u8(1 13 10))

(test-equal "u8vector-normalize-eol:s 9b"
            (u8vector-normalize-eol:s '#u8(1 10) 'cr-lf)
            '#u8(1 13 10))

(test-equal "u8vector-normalize-eol:s 9c"
            (u8vector-normalize-eol:s '#u8(1 13 10) 'cr-lf)
            '#u8(1 13 10))

(test-equal "u8vector-normalize-eol:s 10a"
            (u8vector-normalize-eol:s '#u8(1 13 1) 'cr)
            '#u8(1 13 1))

(test-equal "u8vector-normalize-eol:s 10b"
            (u8vector-normalize-eol:s '#u8(1 10 1) 'cr)
            '#u8(1 13 1))

(test-equal "u8vector-normalize-eol:s 10c"
            (u8vector-normalize-eol:s '#u8(1 13 10 1) 'cr)
            '#u8(1 13 1))

(test-equal "u8vector-normalize-eol:s 11a"
            (u8vector-normalize-eol:s '#u8(1 13 1) 'lf)
            '#u8(1 10 1))

(test-equal "u8vector-normalize-eol:s 11b"
            (u8vector-normalize-eol:s '#u8(1 10 1) 'lf)
            '#u8(1 10 1))

(test-equal "u8vector-normalize-eol:s 11c"
            (u8vector-normalize-eol:s '#u8(1 13 10 1) 'lf)
            '#u8(1 10 1))

(test-equal "u8vector-normalize-eol:s 12a"
            (u8vector-normalize-eol:s '#u8(1 13 1) 'cr-lf)
            '#u8(1 13 10 1))

(test-equal "u8vector-normalize-eol:s 12b"
            (u8vector-normalize-eol:s '#u8(1 10 1) 'cr-lf)
            '#u8(1 13 10 1))

(test-equal "u8vector-normalize-eol:s 12c"
            (u8vector-normalize-eol:s '#u8(1 13 10 1) 'cr-lf)
            '#u8(1 13 10 1))

(test-equal "u8vector-normalize-eol:s 13"
            (u8vector-normalize-eol:s '#u8(13 1 10 2 13 10 3 10) 'cr)
            '#u8(13 1 13 2 13 3 13))

(test-equal "u8vector-normalize-eol:s 14"
            (u8vector-normalize-eol:s '#u8(13 1 10 2 13 10 3 10) 'lf)
            '#u8(10 1 10 2 10 3 10))

(test-equal "u8vector-normalize-eol:s 15"
            (u8vector-normalize-eol:s '#u8(13 1 10 2 13 10 3 10) 'cr-lf)
            '#u8(13 10 1 13 10 2 13 10 3 13 10))

(test-equal "u8vector-normalize-eol:s 16"
            (u8vector-normalize-eol:s '#u8(1 13 2 10 3 13 10 4 10 5) 'cr)
            '#u8(1 13 2 13 3 13 4 13 5))

(test-equal "u8vector-normalize-eol:s 17"
            (u8vector-normalize-eol:s '#u8(1 13 2 10 3 13 10 4 10 5) 'lf)
            '#u8(1 10 2 10 3 10 4 10 5))

(test-equal "u8vector-normalize-eol:s 18"
            (u8vector-normalize-eol:s '#u8(1 13 2 10 3 13 10 4 10 5) 'cr-lf)
            '#u8(1 13 10 2 13 10 3 13 10 4 13 10 5))

(test-end)

