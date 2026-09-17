import { defineConfig } from "vite";

// The webserver expects stable, unhashed filenames (static/main.js and
// static/main.css); it generates its own content-hashed copies at startup.
export default defineConfig({
  build: {
    outDir: "static",
    emptyOutDir: true,
    rollupOptions: {
      input: "js/main.ts",
      output: {
        entryFileNames: "main.js",
        chunkFileNames: "main.js",
        assetFileNames: "[name].[ext]",
      },
    },
  },
});
