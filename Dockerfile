# Copyright (c) 2012-2016 Peter Morgan <peter.james.morgan@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
FROM ubuntu:precise
MAINTAINER Peter Morgan <peter.james.morgan@gmail.com>

ENV TZ=GMT
ENV CODE_LOADING_MODE=interactive

EXPOSE 22 53 80 8080

VOLUME ["/opt/haystack/priv/ssh"]
ENTRYPOINT ["/opt/haystack/bin/haystack"]
CMD ["foreground"]

RUN apt-get update && apt-get install -y \
    wget

RUN wget \
    --no-check-certificate \
    https://packagecloud.io/install/repositories/shortishly/haystack/script.deb.sh

RUN chmod u+x script.deb.sh
RUN ./script.deb.sh

RUN apt-get update && apt-get install -y \
    haystack

RUN rm -rf /var/lib/apt/lists/*
