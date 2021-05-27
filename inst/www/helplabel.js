$('body').on('click', '.label-help-icon',function(e){
  var target = e.currentTarget;
  //$(target).find(".help-below-label").css("display", "block");
  $(target).parent().find(".help-below-label").toggle();
});
