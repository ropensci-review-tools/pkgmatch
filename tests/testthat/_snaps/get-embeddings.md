# raw embeddings

    Code
      emb0 <- httptest2::with_mock_dir("emb_raw", {
        pkgmatch_embeddings_from_pkgs(packages)
      })
    Message
      Generating text embeddings [1 / 2] ...
      Generating text embeddings [2 / 2] ...
      Generating code embeddings ...

