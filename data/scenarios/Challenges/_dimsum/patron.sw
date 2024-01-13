def modifyCart = \cartType. \device.
    result <- scan forward;
    case result return (\item.
        if (item == cartType) {
            use device forward;
            return ();
        } {
            return ();
        };
    );
    end;

def watchSpot =
    watch forward;
    wait 1000;
    modifyCart "full cart" "fork";
    end;

def go =
    watchSpot;
    go;
    end;

go;
