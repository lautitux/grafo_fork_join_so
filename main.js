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

app.ports.renderGraph.subscribe((graph) => {
  mermaid.render("graph-svg", graph).then(({ svg }) => {
    app.ports.svgImage.send(`data:image/svg+xml;base64,${window.btoa(svg)}`);
  });
});

app.ports.export.subscribe((graph) => {
  mermaid.initialize({ theme: "neutral" });
  mermaid.render("graph-svg", graph).then(({ svg }) => {
    const modal = document.getElementById("svg-modal");
    const output = document.getElementById("svg-output");
    output.innerText = svg;
    modal.showModal();
    output.focus();
  });
  mermaid.initialize({ theme: mermaidTheme });
});

app.ports.editorLoadExample.subscribe((example) => {
  editor.session.setValue(example);
});

editor.session.on("change", () => {
  app.ports.editorUpdate.send(editor.session.getValue());
});
