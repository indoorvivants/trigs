package trigs

/** The CLI identifies documents by absolute file path */
given IdCodec[os.Path] = IdCodec[String].imap(os.Path(_))(_.toString)
