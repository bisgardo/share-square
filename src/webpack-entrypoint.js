'use strict';

// Import CSS directly into 'app.js'.
// It will be imported into the HTML from there (presumably through the webpack CSS plugins).
import 'bootstrap/dist/css/bootstrap.min.css';
import { Modal, Tooltip } from 'bootstrap';

// Import all the Elm code.
import { Elm } from './Main.elm';

const app = Elm.Main.init({
    node: document.getElementById('app'),
    flags: {
        environment: process.env.NODE_ENV, // populated with the 'mode' value by webpack 
    },
});

window.addEventListener('shown.bs.modal', event => {
    const modalElement = event.target;
    modalElement.querySelector('select,input').focus();
});
window.addEventListener('hidden.bs.modal', event => {
    const modalId = event.target.id;
    app.ports.modalClosed.send(modalId);
});
app.ports.closeModal.subscribe(id => {
	const element = document.getElementById(id);
    Modal.getInstance(element).hide();
});

// Enable tooltips.
// TODO It doesn't seem like the tooltips have been reliably initialized when the focus call fires.
//      Should be implemented as a MutableObserver instead?
[].slice.call(document.querySelectorAll('[data-bs-toggle="tooltip"]'))
    .forEach(element => new Tooltip(element));
