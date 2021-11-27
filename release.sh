VERSION=`cat VERSION`

CMD="dotnet nuget push bin/Debug/Freedle.$VERSION.nupkg --api-key $NUGET_KEY --source https://api.nuget.org/v3/index.json"
res=`$CMD`
echo $res
