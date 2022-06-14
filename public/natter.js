const apiUrl = 'http://localhost:8001';

function createSpace(name, owner) {
  const data = {name, owner};

  fetch(apiUrl + '/spaces', {
    method: 'POST',
    credentials: 'include',
    body: JSON.stringify(data),
    headers: {
      'Content-Type': 'application/json'
    }
  }).then(response => {
    if (response.ok) {
      return response.json();
    } else {
      throw Error(response.statusText);
    }
  }).then(json => console.log('Created space: ', json.name, json.uri))
    .catch(error => console.log('Error: ', error));
}

window.addEventListener('load', e => {
  document.getElementById('createSpace')
    .addEventListener('submit', processFormSubmit);
});

function processFormSubmit(e) {
  e.preventDefault();
  const spaceName = document.getElementById('spaceName').value;
  const owner = document.getElementById('owner').value;

  createSpace(spaceName, owner);
  return false;
}
