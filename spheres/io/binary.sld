(define-library (spheres/io binary)
 (export default-endian
         default-float-endian

         binary-port?
         open-binary-input-file
         open-binary-output-file
         call-with-binary-input-file
         call-with-binary-output-file
         with-input-from-binary-file
         with-output-to-binary-file

         read-binary-uint8
         read-binary-uint16
         read-binary-uint32
         read-binary-uint64

         read-binary-sint8
         read-binary-sint16
         read-binary-sint32
         read-binary-sint64

         write-binary-uint8
         write-binary-uint16
         write-binary-uint32
         write-binary-uint64

         write-binary-sint8
         write-binary-sint16
         write-binary-sint32
         write-binary-sint64

         read-network-uint16
         read-network-uint32
         read-network-uint64

         read-network-sint16
         read-network-sint32
         read-network-sint64

         write-network-uint16
         write-network-uint32
         write-network-uint64

         write-network-sint16
         write-network-sint32
         write-network-sint64

         read-ber-integer
         write-ber-integer

         read-ieee-float32
         read-ieee-float64

         write-ieee-float32
         write-ieee-float64)

 (include "binary.scm"))
