(in-package #:libsodium-h)

(include #p"/usr/include/sodium/crypto_box.h")

(constant (+box-public-key-bytes+ "crypto_box_PUBLICKEYBYTES"))
(constant (+box-secret-key-bytes+ "crypto_box_SECRETKEYBYTES"))
(constant (+box-nonce-bytes+ "crypto_box_NONCEBYTES"))
(constant (+box-zerobytes+ "crypto_box_ZEROBYTES"))
(constant (+box-boxzerobytes+ "crypto_box_BOXZEROBYTES"))
