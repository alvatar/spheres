(##spheres-load energy: testing)
(##spheres-load crypto: hmac version: (debug))

(test-begin "Hash-based Message Authentication Code")

(define (test-hex p data key expected-result)
  (test-equal (p (hex-string->u8vector data #f) (hex-string->u8vector key #f))
              expected-result))

;; Vectors provided by
;; https://en.wikipedia.org/wiki/Hash-based_message_authentication_code#Examples_of_HMAC_.28MD5.2C_SHA1.2C_SHA256.29
(test-equal (hmac-md5     "" "")
            "74e6f7298a9c2d168935f58c001bad88")
(test-equal (hmac-sha-1   "" "")
            "fbdb1d1b18aa6c08324b7d64b71fb76370690e1d")
(test-equal (hmac-sha-256 "" "")
            "b613679a0814d9ec772f95d778c35fc5ff1697c493715653c6c712144292c5ad")

(test-equal (hmac-md5     "The quick brown fox jumps over the lazy dog" "key")
            "80070713463e7749b90c2dc24911e275")
(test-equal (hmac-sha-1   "The quick brown fox jumps over the lazy dog" "key")
            "de7c9b85b8b78aa6bc8a7a36f70a90701c9db4d9")
(test-equal (hmac-sha-256 "The quick brown fox jumps over the lazy dog" "key")
            "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8")

;; Test vectors from RFC 4231
;; Test case 1, data is "Hi There"
(test-hex hmac-sha-224
          "4869205468657265"
          "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"
          "896fb1128abbdf196832107cd49df33f47b4b1169912ba4f53684b22")
(test-hex hmac-sha-256
          "4869205468657265"
          "0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"
          "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7")
;; Test case 2, key = "Jefe" and data = "what do ya want for nothing?"
(test-hex hmac-sha-224
          "7768617420646f2079612077616e7420666f72206e6f7468696e673f"
          "4a656665"
          "a30e01098bc6dbbf45690f3a7e9e6d0f8bbea2a39e6148008fd05e44")

(test-hex hmac-sha-256
          "7768617420646f2079612077616e7420666f72206e6f7468696e673f"
          "4a656665"
          "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843")
;; Test case 3
(test-hex hmac-sha-224
          "dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "7fb3cb3588c6c1f6ffa9694d7d6ad2649365b0c1f65d69d1ec8333ea")

(test-hex hmac-sha-256
          "dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd"
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "773ea91e36800e46854db8ebd09181a72959098b3ef8c122d9635514ced565fe")
;; Test case 4
(test-hex hmac-sha-224
          "cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd"
          "0102030405060708090a0b0c0d0e0f10111213141516171819"
          "6c11506874013cac6a2abc1bb382627cec6a90d86efc012de7afec5a")

(test-hex hmac-sha-256
          "cdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcdcd"
          "0102030405060708090a0b0c0d0e0f10111213141516171819"
          "82558a389a443c0ea4cc819899f2083a85f0faa3e578f8077a2e3ff46729665b")
;; Test case 5 is about truncation and we don't do that, so we skip that one.
;; Test case 6
(test-hex hmac-sha-224
          "54657374205573696e67204c6172676572205468616e20426c6f636b2d53697a65204b6579202d2048617368204b6579204669727374"
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "95e9a0db962095adaebe9b2d6f0dbce2d499f112f2d2b7273fa6870e")

(test-hex hmac-sha-256
          "54657374205573696e67204c6172676572205468616e20426c6f636b2d53697a65204b6579202d2048617368204b6579204669727374"
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54")
;; Test case 7
(test-hex hmac-sha-224
          "5468697320697320612074657374207573696e672061206c6172676572207468616e20626c6f636b2d73697a65206b657920616e642061206c6172676572207468616e20626c6f636b2d73697a6520646174612e20546865206b6579206e6565647320746f20626520686173686564206265666f7265206265696e6720757365642062792074686520484d414320616c676f726974686d2e"
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "3a854166ac5d9f023f54d517d0b39dbd946770db9c2b95c9f6f565d1")
(test-hex hmac-sha-256
          "5468697320697320612074657374207573696e672061206c6172676572207468616e20626c6f636b2d73697a65206b657920616e642061206c6172676572207468616e20626c6f636b2d73697a6520646174612e20546865206b6579206e6565647320746f20626520686173686564206265666f7265206265696e6720757365642062792074686520484d414320616c676f726974686d2e"
          "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
          "9b09ffa71b942fcb27635fbcd5b0e944bfdc63644f0713938a7f51535c3a35e2")

;; Double-checked using
;; http://myeasywww.appspot.com/utility/free/online/Crypt_Decrypt-MD5-AES-HMAC-SHA-DES-RABBIT/en?command=UTILITY&ID=2
;; and https://quickhash.com/ :
(test-equal (hmac-md5 "Awesomeness."
                      "This string is longer than sixty-four characters, just so you know.")
            "c11692f0b7fc62a76f538e0fe86fb2bb")
(test-equal (hmac-sha-1 "Awesomeness."
                        "This string is longer than sixty-four characters, just so you know.")
            "e2af15ee298e5b7ecc400c9f938c69ba4747b53c")
(test-equal (hmac-sha-256 "Awesomeness."
                          "This string is longer than sixty-four characters, just so you know.")
            "e2315eea89ab6598d8b2698a56eef77c997900d7e46a02e877666323fe7e23cd")

;; Double-checked using https://quickhash.com/ :
;;(test-equal (hmac-crc32   "Awesomeness." "This string is longer than sixty-four characters, just so you know.") "9bab1f86")

(test-end)
