Shiny.addCustomMessageHandler('upload_msg', function(msg) {
  var target = $('#module_1-file2_progress').children()[0];
  target.innerHTML = msg;
});