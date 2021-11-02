'use strict';

// Import CSS directly into 'app.js'.
// It will be imported into the HTML from there (presumably through the webpack CSS plugins).
import 'bootstrap/dist/css/bootstrap.min.css';
import 'bootstrap-icons/font/bootstrap-icons.css';
import { Modal, Tooltip } from 'bootstrap';

// Import all the Elm code.
import { Elm } from './Main.elm';

// Manage tooltips: Listen for DOM mutation events that nodes with tooltips
// are being added or removed.
const observer = new MutationObserver(mutations =>
    mutations.forEach(mutation => {
        mutation.addedNodes.forEach(node => {
            if (node.nodeType === Node.ELEMENT_NODE) {
                node.querySelectorAll('[data-bs-toggle="tooltip"]')
                    .forEach(element => new Tooltip(element));
            }
        });
        mutation.removedNodes.forEach(node => {
            if (node.nodeType === Node.ELEMENT_NODE) {
                node.querySelectorAll('[data-bs-toggle="tooltip"]')
                    .forEach(element => Tooltip.getInstance(element).dispose());
            }
        });
    })
).observe(document.body, {childList: true, subtree: true});

console.log(observer);
const app = Elm.Main.init({
    node: document.getElementById('app'),
    flags: {
        environment: process.env.NODE_ENV, // populated with the 'mode' value by webpack 
    },
});

window.addEventListener('shown.bs.modal', event => {
    const modalElement = event.target;
    const firstFormElement = modalElement.querySelector('select,input');
    if (firstFormElement) firstFormElement.focus();
});
window.addEventListener('hidden.bs.modal', event => {
    const modalId = event.target.id;
    app.ports.modalClosed.send(modalId);
});
app.ports.closeModal.subscribe(id => {
	const element = document.getElementById(id);
    Modal.getInstance(element).hide();
});
