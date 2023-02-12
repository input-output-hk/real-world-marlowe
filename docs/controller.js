function openLink(path) {
  if (window.location.origin == "file://")
    window.open("../" + path, 'rwm');
  else
    window.open("https://github.com/bwbush/real-world-marlowe/blob/main/" + path, 'rwm');
}
