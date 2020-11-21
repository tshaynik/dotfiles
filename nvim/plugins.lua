local iron = require('iron')

iron.core.add_repl_definitions {
  rust = {
    mycustom = {
      command = {"evcxr"}
    }
  }
}

iron.core.set_config {
  preferred = {
    python = "ipython",
    clojure = "lein"
  }
}
