const body = document.body;

body.addEventListener("click", async () => {
  const stream = await navigator.mediaDevices.getDisplayMedia({ video: true });
  const mime = MediaRecorder.isTypeSupported("video/webm; codecs=vp9")
    ? "video/webm; codecs=vp9"
    : "video/webm";
  // Record
  const mediaRecorder = new MediaRecorder(stream, { mimeType: mime });
  const chunks: Blob[] = [];
  mediaRecorder.addEventListener("dataavailable", (e) => {
    chunks.push(e.data);
  });
  // Stop
  mediaRecorder.addEventListener("stop", () => {
    let blob = new Blob(chunks, { type: chunks[0].type });

    let url = URL.createObjectURL(blob);

    let a = document.createElement("a");
    a.href = url;
    a.download = "video.webm";
    a.click();
  });

  // Start manually
  mediaRecorder.start();
});
