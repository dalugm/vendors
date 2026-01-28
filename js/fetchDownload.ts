const url = "https://sample.example.file";
const authHeader = "Bearer **************";

const options: RequestInit = {
  headers: {
    Authorization: authHeader,
  },
};

fetch(url, options)
  .then((res) => res.blob())
  .then((blob) => {
    const file = window.URL.createObjectURL(blob);
    window.location.assign(file);
  });
