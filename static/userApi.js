
var postUser = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/user'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}

var getUser = function(onSuccess, onError)
{
  $.ajax(
    { url: '/user'
    , success: onSuccess
    , error: onError
    , type: 'GET'
    });
}

var postPlay = function(body, onSuccess, onError)
{
  $.ajax(
    { url: '/play'
    , success: onSuccess
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , error: onError
    , type: 'POST'
    });
}
