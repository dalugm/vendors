let body = document.body;

body.addEventListener('click', async () => {
  var stream = await navigator.mediaDevices.getDisplayMedia({ video: true });
  var mime = MediaRecorder.isTypeSupported('video/webm; codecs=vp9')
    ? 'video/webm; codecs=vp9'
    : 'video/webm';
  // Record.
  let mediaRecorder = new MediaRecorder(stream, { mimeType: mime });
  let chunks = [];
  mediaRecorder.addEventListener('dataavailable', function (e) {
    chunks.push(e.data);
  });
  // Stop.
  mediaRecorder.addEventListener('stop', () => {
    let blob = new Blob(chunks, { type: chunks[0].type });

    let url = URL.createObjectURL(blob);

    let a = document.createElement('a');
    a.href = url;
    a.download = 'video.webm';
    a.click();
  });

  // Start manually.
  mediaRecorder.start();
});
