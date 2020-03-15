  
window.addEventListener('load', () => {
  initUI();
});

function handleReflexVaadinEvent(ev) {
  // TODO: serialize the calls so no parallel calls are made
  const bodyElement = document.getElementsByTagName("BODY")[0]; 
  const sid = bodyElement.attributes.getNamedItem("data-reflex-vaadin-sid").value;
  var deta;
  if (ev.type == "input") {
    deta = {value: ev.target.value};    
  } else {
    deta = ev.detail;
  }
  if (ev.target.hasAttribute(`data-frp-snapshot-${ev.type}`)) {
    var snapshotNum = Number(ev.target.getAttribute(`data-frp-snapshot-${ev.type}`)) + 1;
    ev.target.setAttribute(`data-frp-snapshot-${ev.type}`, snapshotNum)
    deta.snapshot = snapshotNum;
  }
  fetch(`/events?sid=${sid}`, {
    headers: {
      'Accept': 'application/json',
      'Content-Type': 'application/json'      
    },
    method: 'POST',
    body: JSON.stringify({
      _frontendEvent_elementId: parseInt(ev.srcElement.id), 
      _frontendEvent_eventName: ev.type, 
      _frontendEvent_details: deta
    })
  })
  .catch(function(res){ console.log(res) });
}

function initUI() {
  addListeners();
  getEvents();
}

function getEvents() {
  const bodyElement = document.getElementsByTagName("BODY")[0]; 
  const sid = bodyElement.attributes.getNamedItem("data-reflex-vaadin-sid").value;
  const mainApplication = document.getElementById("reflex-vaadin-main-app");
  const snapshot = mainApplication.attributes.getNamedItem("data-reflex-vaadin-snapshot").value;
  fetch(`/events?sid=${sid}&snapshot=${snapshot}`, {
    headers: {
      'Accept': 'text/html',
    },
    method: 'GET'
  })
  .then((response) => response.json())
  .then((result) => {
    mainApplication.setAttribute("data-reflex-vaadin-snapshot", result[0]);
    processDeltas(mainApplication, result[1]);
  })
  .then(addListeners) // TODO Add listeners only to new elements
  .then(getEvents)
  .catch(function(error) {
    console.log(error);
  })
}

function processDeltas(element, deltas) {
  deltas.forEach(function(delta) {
    switch(delta.tag) {
      case "ModifyChildren": 
      var elementDeltaIndex = delta.contents[0];
      processDeltas(element.children[element.children.length - elementDeltaIndex - 1], delta.contents[1]);
        break;
      case "AddNode":
        var elementDeltaIndex = delta.contents[0];
        var nodeDetails = delta.contents[1];
        var newNode;
        switch(nodeDetails.type) {
          case "element":
            newNode = document.createElement(nodeDetails.nodeName);
            nodeDetails.attributes.forEach((attr) => newNode.setAttribute(attr[0], attr[1]));
            newNode.innerHTML = nodeDetails.innerHtml
            break;
          case "text":
            newNode = document.createTextNode(nodeDetails.content);
            break
        }
        if(elementDeltaIndex == 0) {
          element.appendChild(newNode);
        } else {
          element.insertBefore(newNode, element.children[element.children.length - elementDeltaIndex]);
        }
        break;
      case "RemoveNode":
        var elementDeltaIndex = delta.contents;
        element.children[element.children.length - elementDeltaIndex - 1].remove();
        break;
      case "InnerHtml":
        var elementDeltaIndex = delta.contents[0];
        var theText = delta.contents[1];
        element.children[element.children.length - elementDeltaIndex - 1].innerHTML = theText;
        break;
      case "PutAttribute":
        var elementDeltaIndex = delta.contents[0];
        var attributeName = delta.contents[1];
        if (attributeName.startsWith('data-frp-snapshot-')) {
          break;
        }
        var childElement = element.children[element.children.length - elementDeltaIndex - 1];
        if (childElement.hasAttribute(`data-frp-event-${attributeName}`)) {
          break;
        }
        var value = delta.contents[2];
        if(attributeName.startsWith('data-frp-event-')) {
          var values = value.split("|");
          var realValueAttribute = attributeName.substring(15);
          var snapshotAttributeName = `data-frp-snapshot-${values[0]}`;
          var inputSnapshot = Number(values[1]);
          var realValue = values.slice(2).join('');
          var currentSnapshot = Number(childElement.getAttribute(snapshotAttributeName));
          if(currentSnapshot == inputSnapshot) {
            childElement.setAttribute(realValueAttribute, realValue);
          } else if(currentSnapshot < inputSnapshot) {
            // How can it be? it is working like a mirror?
            childElement.setAttribute(realValueAttribute, realValue);
            childElement.setAttribute(snapshotAttributeName, inputSnapshot);
          } else {
            console.log("Bypassing update");
            console.log(delta);
          }
          childElement.setAttribute(attributeName, value);
        } else {
          childElement.setAttribute(attributeName, value);
        }
        break;
      case "RemoveAttribute":
        var elementDeltaIndex = delta.contents[0];
        var attributeName = delta.contents[1];
        element.children[element.children.length - elementDeltaIndex - 1].removeAttribute(attributeName);
        break;
    }
  });
}

function addListeners() {
  const elements = document.querySelectorAll('[data-reflex-vaadin-events]');
  elements.forEach(function(nod) {
    const eventNode = nod.attributes.getNamedItem('data-reflex-vaadin-events');
    const events = eventNode.value.split(' ');
    events.forEach(function(eventName) {
      nod.addEventListener(eventName, handleReflexVaadinEvent);
    });
  });
}