'use strict';

// Import CSS directly into 'app.js'. It will be automagically imported into the HTML from there.
import 'bootstrap/dist/css/bootstrap.min.css';

// Import all the Elm code.
import { Elm } from './Main.elm';

const app = Elm.Main.init({
    node: document.getElementById('app'),
    flags: {
        environment: process.env.NODE_ENV, // populated with the 'mode' value by webpack 
    },
});
