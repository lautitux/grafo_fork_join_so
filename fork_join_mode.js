ace.define(
  "ace/mode/fork_join",
  [
    "require",
    "exports",
    "module",
    "ace/lib/oop",
    "ace/mode/text",
    "ace/mode/text_highlight_rules",
  ],
  function (require, exports, module) {
    "use strict";

    var oop = require("../lib/oop");
    var TextMode = require("./text").Mode;
    var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;

    // 1. Highlight Rules Definition
    var MinimalHighlightRules = function () {
      this.$rules = {
        start: [
          // Comments: Start with ; until the end of the line (per instruction)
          {
            token: "comment",
            regex: ";.*$",
          },
          // Labels: <IDENTIFIER> ":" 
          // (Note: Checked before keywords/identifiers to capture the colon)
          {
            token: "entity.name.function",
            regex: "[a-zA-Z_][a-zA-Z0-9_]*:",
          },
          // Keywords: QUIT, FORK, GOTO, JOIN
          {
            token: "keyword",
            regex: "\\b(?:QUIT|FORK|GOTO|JOIN)\\b",
            caseInsensitive: true
          },
          // Integers: [0-9]+
          {
            token: "constant.numeric",
            regex: "\\b\\d+\\b",
          },
          // Operators: = (counter) and , (binary/join)
          {
            token: "keyword.operator",
            regex: "[=,]",
          },
          // Identifiers: [a-zA-Z_][a-zA-Z0-9_]*
          {
            token: "variable",
            regex: "[a-zA-Z_][a-zA-Z0-9_]*\\b",
          },
          // Whitespace
          {
            token: "text",
            regex: "\\s+",
          }
        ],
      };
    };
    oop.inherits(MinimalHighlightRules, TextHighlightRules);

    // 2. Mode Definition
    var Mode = function () {
      this.HighlightRules = MinimalHighlightRules;
      this.$behaviour = this.$defaultBehaviour;
    };
    oop.inherits(Mode, TextMode);

    (function () {
      this.id = "ace/mode/fork_join";
    }).call(Mode.prototype);

    exports.Mode = Mode;
  }
);