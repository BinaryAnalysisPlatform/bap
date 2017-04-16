// This function is used to return the content of a file
// The output is string
function readFrom(filePath){
  xmlhttp = new XMLHttpRequest();
  xmlhttp.overrideMimeType('text/plain');
  xmlhttp.open("GET",filePath,false);
  xmlhttp.send(null);
  fileContent = xmlhttp.responseText;
  fileArray = fileContent.split('\n')
  n = fileArray.length;
  return fileContent;
}





