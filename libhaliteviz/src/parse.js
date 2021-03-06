import parseWorker from "worker-loader!./parseWorker.js";

export function parseReplay(buffer) {
    const decoderPromise = window.TextDecoder ?
          Promise.resolve({ TextDecoder: window.TextDecoder }) :
          import(/* webpackChunkName: "text-encoding" */ "text-encoding");
    return new Promise((resolve, reject) => {
        try {
            const startTime = Date.now();
            const worker = new parseWorker();
            worker.onmessage = function (e) {
                const inflated = e.data;
                const inflatedTime = Date.now();
                const arr = new Uint8Array(inflated);
                if (arr[0] === 0) {
                    // Decompression failed
                    reject();
                    return;
                }

                decoderPromise.then((module) => {
                    const decoded = new module.TextDecoder("utf-8").decode(arr);
                    try {
                        const replay = JSON.parse(decoded);
                        const finishTime = Date.now();
                        console.info(`Decoded compressed replay in ${finishTime - startTime}ms, inflating took ${inflatedTime - startTime}ms, decoding took ${finishTime - inflatedTime}ms.`);
                        resolve(replay);
                    }
                    catch (e) {
                        console.error("Could not decompress replay.");
                        console.error(e);
                        reject(e);
                    }
                });
            };
            worker.postMessage(buffer, [buffer]);
            if (buffer.byteLength) {
                console.warn("Transferrables not supported, could not decode without copying data!");
            }
        }
        catch (e) {
            console.error(e);
            reject(e);
        }
    });
}
