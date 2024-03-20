/**
 * The `express` server for the question asking service.
 * All endpoints return JSON as output.
 */

import express, { json, Request, Response } from 'express';
import morgan from 'morgan';
import { submit, like, dismiss, questions } from './questions';

const app = express();
app.use(json());
// For debugging purposes - logs http requests
app.use(morgan('dev'));

/**
 * Endpoint: '/question/submit'
 * Method: POST
 * Parameter: (question : string)
 * Output: { success : boolean, id : number}
 *
 * Submit the given question. The success output should be true if the question
 * was successfully posted and false otherwise. If success is true then id should
 * be a valid question id.
 */

// Write this endpoint here


/**
 * Endpoint: '/questions'
 * Method: GET
 * Parameter: ()
 * Output: Question[]
 *
 * List all questions that have been submitted. The ordering of the questions is
 * the same as defined in the backend.
 */

// Write the endpoint here


/**
 * Endpoint: '/question/like'
 * Method: POST
 * Parameter: (id : number)
 * Output: { success : boolean }
 *
 * Like question with the given id. The success output should be true if the id
 * was a valid question id and false otherwise.
 */

// Write the endpoint here


/**
 * Endpoint: '/question/dismiss'
 * Method: DELETE
 * Parameter: (id : number)
 * Output: { success : boolean }
 *
 * Dismiss the question with the given id. The success output should be true if
 * the id was a valid question id and false otherwise.
 */

// Write the endpoint here


app.listen(8000, () => console.log('Server listening on port 8000'));
