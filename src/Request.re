type t('a) = {
  body: 'a,
  path: string,
  headers: Header.Map.t(string),
  method: HttpMethod.t,
  isSecure: bool,
};