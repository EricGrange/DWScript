type TSiteData=Record
SousSite: String; 
end;

type SitesTreeInformations=static class
public
class function ForSite (site : String) : TSiteData;
end;

class function SitesTreeInformations.ForSite(site: String): TSiteData;
begin
Result.SousSite := 'toto';
end;

var siteData := TSiteData;
siteData := SitesTreeInformations.ForSite('hello');