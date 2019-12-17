
var postState = function(onSuccess, onError)
{
  $.ajax(
    { url: '/state'
    , success: onSuccess
    , error: onError
    , type: 'POST'
    });
}

var getState = function(onSuccess, onError)
{
  $.ajax(
    { url: '/state'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}
