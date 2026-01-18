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

app.ports.export.subscribe(async (graph) => {
  mermaid.initialize({ theme: "neutral" });
  const { svg } = await mermaid.render("graph-svg", graph);
  const pngUrl = await svgStringToPngBase64(svg);
  window.open(pngUrl, "_blank");
  mermaid.initialize({ theme: mermaidTheme });
});

app.ports.editorLoadExample.subscribe((example) => {
  editor.session.setValue(example);
});

editor.session.on("change", () => {
  app.ports.editorUpdate.send(editor.session.getValue());
});

// Function provided by Google Gemini to convert an SVG into a PNG base 64 url
async function svgStringToPngBase64(svgString, width = 2048, height = 2048) {
  return new Promise((resolve, reject) => {
    // 1. Create a Blob from the SVG string
    const svgBlob = new Blob([svgString], {
      type: "image/svg+xml;charset=utf-8",
    });
    const url = URL.createObjectURL(svgBlob);

    const img = new Image();
    img.onload = () => {
      // 2. Prepare the Canvas
      const canvas = document.createElement("canvas");
      canvas.width = width;
      canvas.height = height;

      const ctx = canvas.getContext("2d");

      // Optional: Clear canvas for transparency or set a background color
      ctx.clearRect(0, 0, width, height);

      // 3. Draw the image
      ctx.drawImage(img, 0, 0, width, height);

      // 4. Cleanup and Resolve
      URL.revokeObjectURL(url);
      resolve(canvas.toDataURL("image/png"));
    };

    img.onerror = (err) => {
      URL.revokeObjectURL(url);
      reject(
        new Error("Failed to render SVG. Ensure the string is valid XML."),
      );
    };

    img.src = url;
  });
}
