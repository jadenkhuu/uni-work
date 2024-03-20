import { globalToken } from './main.js'
import { BACKEND_PORT } from './config.js';

/**
 * Given a js file object representing a jpg or png image, such as one taken
 * from a html file input element, return a promise which resolves to the file
 * data as a data url.
 * More info:
 *   https://developer.mozilla.org/en-US/docs/Web/API/File
 *   https://developer.mozilla.org/en-US/docs/Web/API/FileReader
 *   https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics_of_HTTP/Data_URIs
 * 
 * Example Usage:
 *   const file = document.querySelector('input[type="file"]').files[0];
 *   console.log(fileToDataUrl(file));
 * @param {File} file The file to be read.
 * @return {Promise<string>} Promise which resolves to the file as a data url.
 */
export function fileToDataUrl(file) {
    const validFileTypes = [ 'image/jpeg', 'image/png', 'image/jpg' ]
    const valid = validFileTypes.find(type => type === file.type);
    // Bad data, let's walk away.
    if (!valid) {
        throw Error('provided file is not a png, jpg or jpeg image.');
    }
    
    const reader = new FileReader();
    const dataUrlPromise = new Promise((resolve,reject) => {
        reader.onerror = reject;
        reader.onload = () => resolve(reader.result);
    });
    reader.readAsDataURL(file);
    return dataUrlPromise;
}

export const api_call_post = (path, body, authed=false) => {
    return new Promise((resolve, reject) => {
        fetch(`http://localhost:${BACKEND_PORT}/` + path, {
            method: 'POST',
            body: JSON.stringify(body),
            headers: {
                'Content-type': 'application/json',
                'Authorization': authed ? `Bearer ${globalToken}` : undefined
            },
        })
        .then((response) => response.json())
        .then((body) => {
            if (body.error) {
                reject(body.error)
            } else {
                resolve(body);
            }
        });
    });
};

export const api_call_get = (path, body, authed=false) => {
	return new Promise((resolve, reject) => {
		fetch(`http://localhost:${BACKEND_PORT}/` + path, {
			method: 'GET',
			headers: {
				'Content-type': 'application/json',
				'Authorization': authed ? `Bearer ${globalToken}` : undefined
			}
		})
		.then((response) => response.json())
		.then((body) => {
			if (body.error) {
				reject(body.error);
			} else {
				resolve(body);
			}
		});
	});
};
export const api_call_delete = (path, body, authed=false) => {
	return new Promise((resolve, reject) => {
		fetch(`http://localhost:${BACKEND_PORT}/` + path, {
			method: 'DELETE',
			headers: {
				'Content-type': 'application/json',
				'Authorization': authed ? `Bearer ${globalToken}` : undefined
			}
		})
		.then((response) => response.json())
		.then((body) => {
			if (body.error) {
				reject(body.error);
			} else {
				resolve(body);
			}
		});
	});
};

export const api_call_put = (path, body, authed=false) => {
	return new Promise((resolve, reject) => {
		fetch(`http://localhost:${BACKEND_PORT}/` + path, {
			method: 'PUT',
            body: JSON.stringify(body),
			headers: {
				'Content-type': 'application/json',
				'Authorization': authed ? `Bearer ${globalToken}` : undefined
			}
		})
		.then((response) => response.json())
		.then((body) => {
			if (body.error) {
				reject(body.error);
			} else {
				resolve(body);
			}
		});
	});
};

export function format_date(time) {
    const date = new Date(time);

    const day = String(date.getUTCDate()).padStart(2, '0');
    const month = String(date.getUTCMonth() + 1).padStart(2, '0');
    const year = String(date.getUTCFullYear()).slice(-2);

    const hours = String(date.getUTCHours()).padStart(2, '0');
    const minutes = String(date.getUTCMinutes()).padStart(2, '0');
    const seconds = String(date.getUTCSeconds()).padStart(2, '0');

    return `${hours}:${minutes}:${seconds} ${day}/${month}/${year}`;

    // return `${hours}:${minutes} ${day}/${month}/${year}`;
};

export function show_error(message) {
    document.getElementById("error-message").textContent = message;
    document.getElementById("page-error").style.display = "flex";
};

export function close_error() {
    document.getElementById("page-error").style.display = "none";
};


// export const show_error = (msg) => {
//     // for (const page of document.querySelectorAll('.page-block')) {
//     //     page.style.display = 'none';
//     // }
//     // document.getElementById("page-error").style.display = "flex";
//     // errorMessage.innerHTML = `
//     // <h2>${msg}</h2>
//     // `
// }

