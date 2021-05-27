
$('body').on('change', '#input_type',function(e){
  var val = e.currentTarget.value;
  if (val === "n") {
    $("#input_fields_div").css("display", "none");
  } else {
    $("#input_fields_div").css("display", "block");
  }
});
