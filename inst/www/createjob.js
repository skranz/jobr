$('body').on('change', '#response_type',function(e){
  var val = e.currentTarget.value;
  if (val === "m") {
    $("#response_items_div").css("display", "block");
  } else {
    $("#response_items_div").css("display", "none");
  }
});

$('body').on('change', '#input_type',function(e){
  var val = e.currentTarget.value;
  if (val === "n") {
    $("#input_items_div").css("display", "none");
  } else {
    $("#input_items_div").css("display", "block");
  }
});
