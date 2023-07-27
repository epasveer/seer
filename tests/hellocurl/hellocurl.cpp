#include <stdio.h>
#include <curl/curl.h>

int main() {

    printf("test1\n");

    CURL* curl; CURLcode res;
    curl = curl_easy_init();

    if (curl) {

        curl_easy_setopt(curl, CURLOPT_URL, "http://example.com/");

        /* Perform the request, res will get the return code */
        res = curl_easy_perform(curl);

        /* Check for errors */
        if (res != CURLE_OK) {
            fprintf(stderr, "curl_easy_perform() failed: %s\n", curl_easy_strerror(res));
        }

        /* always cleanup */
        curl_easy_cleanup(curl);
    }

    return 0;
}

