"use strict";

const app = Elm.Main.init({ node: document.getElementById("elm") });

const darkMode =
  window.matchMedia &&
  window.matchMedia("(prefers-color-scheme: dark)").matches;
const mermaidTheme = darkMode ? "dark" : "neutral";

const editor = ace.edit("editor");
editor.setOption("showPrintMargin", false);
editor.setOptions({
  fontSize: "11pt",
  mode: "ace/mode/fork_join",
  selectionStyle: "text",
});
if (darkMode) {
  editor.setTheme("ace/theme/cloud_editor_dark");
}

mermaid.initialize({ theme: mermaidTheme });

function graphToMermaidJS(graph, direction = "TD") {
  const lines = [`graph ${direction}`];
  const nodeIdMap = new Map();
  let nextId = 0;

  const formatNode = (name) => {
    if (!nodeIdMap.has(name)) {
      nodeIdMap.set(name, nextId++);
    }
    return `${nodeIdMap.get(name)}["${name}"]`;
  };

  for (const [parent, children] of Object.entries(graph)) {
    if (parent !== "") {
      const parentNode = formatNode(parent);
      const childString = children.map((v) => formatNode(v)).join(" & ");
      if (childString) {
        lines.push(`\t${parentNode} --> ${childString}`);
      } else {
        lines.push(`\t${parentNode}`);
      }
    }
  }

  return lines.join("\n");
}

let graphCode = "";

app.ports.renderGraph.subscribe((message) => {
  graphCode = graphToMermaidJS(message);
  mermaid.render("graph-svg", graphCode).then(({ svg }) => {
    app.ports.loadSVG.send(`data:image/svg+xml;base64,${window.btoa(svg)}`);
  });
});

app.ports.exportAs.subscribe((message) => {
  if (!graphCode) return;
  mermaid.initialize({ theme: "neutral" });
  if (message === "svg") {
    mermaid.render("graph-svg", graphCode).then(({ svg }) => {
      const modal = document.getElementById("svg-modal");
      const output = document.getElementById("svg-output");
      output.innerText = svg;
      modal.showModal();
      output.focus();
    });
  } else {
    console.error("Unsupported format.");
  }
  mermaid.initialize({ theme: mermaidTheme });
});

editor.session.on("change", () => {
  app.ports.editorUpdate.send(editor.session.getValue());
});
