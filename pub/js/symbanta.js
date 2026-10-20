/* symbanta
*
* Copyright © Josep Bigorra <jjbigorra@gmail.com>
*
* symbanta is free software: you can redistribute it and/or modify
* it under the terms of the GNU Affero General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* symbanta is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU Affero General Public License for more details.
*
* You should have received a copy of the GNU Affero General Public License
* along with symbanta.  If not, see <https://www.gnu.org/licenses/>.
*/

// @ts-check
"use strict";

(function () {
    /**
     * @typedef {"page-view" | "heartbeat" | "visibility-change"} EventName
     */

    /**
     * @typedef {Object} EventPayload
     * @property {string} domain
     * @property {EventName} name
     * @property {string} sessionId
     * @property {string} userAgent
     * @property {string} url
     * @property {string | null} referrer
     * @property {number} screenWidth
     * @property {number} screenHeight
     * @property {string} language
     */

    // Ensure strict casting of the script element
    /** @type {HTMLScriptElement | null} */
    const scriptTag = (document.currentScript instanceof HTMLScriptElement ? document.currentScript : null)
        || document.querySelector('script[src*="symbanta"]');

    // Read config options from the <script> tag attributes
    const SYMBANTA_ENDPOINT = scriptTag?.getAttribute("data-endpoint") || "https://api.symbanta.jointhefreeworld.org/v1/events/register";
    const DOMAIN_NAME = scriptTag?.getAttribute("data-domain") || window.location.hostname;

    // Safer integer parsing to avoid NaN bypassing the default fallback
    const rawInterval = scriptTag?.getAttribute("data-interval");
    const parsedInterval = rawInterval ? parseInt(rawInterval, 10) : NaN;
    const HEARTBEAT_INTERVAL_MS = Number.isNaN(parsedInterval) || parsedInterval <= 0 ? 4000 : parsedInterval;

    /**
     * crypto.randomUUID() is only available in secure contexts (HTTPS).
     * This provides a safe fallback if it is missing.
     * @returns {string}
     */
    const generateUUID = () => {
        if (typeof crypto !== "undefined" && crypto.randomUUID) {
            return crypto.randomUUID();
        }
        return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, (c) => {
            const r = Math.random() * 16 | 0;
            const v = c === 'x' ? r : (r & 0x3 | 0x8);
            return v.toString(16);
        });
    };

    // Get or create a session ID limited to current tab lifecycle
    let sessionId = sessionStorage.getItem("symbanta_session_id");
    if (!sessionId) {
        sessionId = generateUUID();
        sessionStorage.setItem("symbanta_session_id", sessionId);
    }

    /**
     * Constructs the payload object
     * @param {EventName} eventName
     * @returns {EventPayload}
     */
    const createPayload = (eventName) => ({
        domain: DOMAIN_NAME,
        name: eventName,
        sessionId: /** @type {string} */ (sessionId),
        userAgent: navigator.userAgent,
        url: window.location.href,
        referrer: document.referrer || null,
        screenWidth: window.screen.width,
        screenHeight: window.screen.height,
        language: navigator.language
    });

    /**
     * Transmits the event to the endpoint
     * @param {EventName} eventName
     */
    const sendEvent = (eventName) => {
        try {
            const payload = createPayload(eventName);
            const blob = new Blob([JSON.stringify(payload)], {type: "application/json"});

            let queued = false;

            // Feature detect sendBeacon (not available in some older browsers or strict webviews)
            if (typeof navigator.sendBeacon === "function") {
                queued = navigator.sendBeacon(SYMBANTA_ENDPOINT, blob);
            }

            // Fallback to fetch if sendBeacon is unavailable or fails
            if (!queued && typeof fetch === "function") {
                fetch(SYMBANTA_ENDPOINT, {
                    method: "POST",
                    headers: {"Content-Type": "application/json"},
                    body: JSON.stringify(payload),
                    keepalive: true
                }).catch(() => { /* Fail silently */
                });
            }
        } catch (error) {
            // Prevent analytics errors from breaking the host page
            console.warn("Symbanta analytics error:", error);
        }
    };

    // Initial page view
    sendEvent("page-view");

    // Periodic heartbeat (only when tab is visible)
    setInterval(() => {
        if (document.visibilityState === "visible") {
            sendEvent("heartbeat");
        }
    }, HEARTBEAT_INTERVAL_MS);

    // Final heartbeat when tab/page is hidden or closed
    document.addEventListener("visibilitychange", () => {
        if (document.visibilityState === "hidden") {
            sendEvent("visibility-change");
        }
    });
})();
